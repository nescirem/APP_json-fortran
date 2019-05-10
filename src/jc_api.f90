!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module check API version.
!

module jc_api_mod

    use json_module, CK => json_CK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code,&
                                            grid_file,grid_type
    use functions,                      only: clean_str,quote_str
    use output_mod
                                        
    implicit none

    private
    public :: jc_api

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_api                                                   !
    !-------------------------------------------------------------------+
    
    use common_data,            only: api_version,file_version,log_level,log_file
    
    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    
    character(len=16)                           :: main_version,sub_version
    character(kind=CK,len=:),allocatable        :: str_temp,log_path,log_file_name
    logical                                     :: found 
    
    
    write( main_version,"(G0)" ) api_version%main
    write( sub_version,"(G0)" ) api_version%sub
    api_version%string = clean_str(main_version)//'.'//clean_str(sub_version)
    
    call json%initialize()
    
    ! parse the json file
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        write( error_unit,"(G0)" )  'An error occurred during parse JSON file'
        write( error_unit,"(G0)" )  ''
        stop                        'Program terminated.'
    else
        call core%initialize()
        if ( core%failed() ) then
            call core%print_error_message( error_unit )
            write( error_unit,"(G0)" )  'An error occurred during json core initialize'
            write( error_unit,"(G0)" )  ''
            stop                        'Program terminated.'
        end if
    end if
    
    ! log level
    call json%get( 'log.level', str_temp, found )
    if ( found ) then
        select case ( str_temp )! 0="none" | 1="error" | 2="warning" | 3="info" | 4 ="debug"
        case ( 'none' )
            log_level = 0
        case ( 'error' )
            log_level = 1
        case ( 'warning' )
            log_level = 2
        case ( 'info' )
            log_level = 3
        case ( 'debug' )
            log_level = 4
        case default
            write( error_unit,"(G0)" ) 'Unknown log level, use DEFAULT "warning" instead'
            log_level = 2
        end select
    else !if ( .not.found ) then
        log_level = 2   ! default log level is "warning"
    end if
    
    ! log file
    if ( log_level>0 ) then
        call json%get( 'log.path', log_path, found )
        if ( .not.found ) log_path = './'
        call json%get( 'log.filename', log_file_name, found )
        if ( .not.found ) then
            select case ( log_level )
            case ( 1 )
                log_file_name = 'api_error.log'
            case ( 2 )
                log_file_name = 'api_warning.log'
            case ( 3 )
                log_file_name = 'api_info.log'
            case ( 4 )
                log_file_name = 'api_debug.log'
            end select
        end if
        log_file = log_path//log_file_name
    end if
    
    call json%get( 'apiVersion', file_version%string, found )
    if ( found ) call parse_vesion()

    call firsttime_error_out()
    call check_version()
    
    ! clean up
    call core%destroy()
    if ( core%failed() ) call core%print_error_message( error_unit )
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_api
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine parse_vesion                                             !
    !-------------------------------------------------------------------+
    
    use common_data,            only: file_version
    
    implicit none
    
    integer         :: i,length
    
    length = len(clean_str(file_version%string))
    
    i = scan(file_version%string,'.')
    
    read( file_version%string(1:i-1),* ) file_version%main
    read( file_version%string(i+1:length),* ) file_version%sub
    
    end subroutine parse_vesion
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine check_version                                            !
    !-------------------------------------------------------------------+
    
    use common_data,            only: api_version,file_version,exit_if_error
    
    implicit none
    
    if ( api_version%main > file_version%main ) then
        call warning_out( 'Current API version '//quote_str(api_version%string)&
                //' is higher than the file API version'//quote_str(file_version%string)  )
    else if ( api_version%main == file_version%main ) then
        if ( api_version%sub > file_version%sub ) then
            call warning_out( 'Current API version '//quote_str(api_version%string)&
                    //' is higher than the file API version'//quote_str(file_version%string)  )
        else if ( api_version%sub < file_version%sub ) then
            call error_out( 'Current API version '//quote_str(api_version%string)&
                    //' is lower than the file API version'//quote_str(file_version%string),exit_if_error )
        else !if ( api_version%sub == file_version%sub ) then
            call info_out( 'Current API version '&
                    //quote_str(api_version%string)//' is identical with the file API version' )
        end if
    else !if ( api_version%main < file_version%main ) then
        call error_out( 'Current API version '//quote_str(api_version%string)&
                //' is lower than the file API version'//quote_str(file_version%string),exit_if_error )
    end if
    
    end subroutine check_version
    
    !====================================================================
    
    
end module jc_api_mod
