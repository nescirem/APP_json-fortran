!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Module print error message for debug.
!
    
module jc_error_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: log_file,log_level

    implicit none

    private
    
    interface error_out
        module procedure jc_error_out
    end interface

    public :: error_out,firsttime_error_out

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_error_out ( err_message,terminate )                   !
    !-------------------------------------------------------------------+
    
    use common_data,                    only: error_code
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: err_message
    logical,optional,intent(in)         :: terminate
    character(len=32)                   :: error_code_str
    
    integer,parameter                   :: set_len = 45
    logical,parameter                   :: default_terminate = .false.
    
    integer                             :: err_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate,l_terminate,alive
    
    if ( log_level>=1 ) then
        
        ! output to log file
        open (  NEWUNIT=unit,FILE=log_file,STATUS='OLD',POSITION='APPEND',IOSTAT=istat )
        write (unit,"(A)") '[ERROR] '//clean_str(err_message)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
        
        
        ! check if terminate argument is given
        if (  present(terminate) ) then
            l_terminate = terminate
        else
            l_terminate = default_terminate
        end if
    
        ! scatter the error message string if it is too long
        err_message_len = len( clean_str(err_message) )
        nr_parts = real(err_message_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts == 1 ) separate = .false.
    
        ! error output
        write( error_code_str,* ) error_code
        write( error_unit,"(A)" )   ''
    
        ! display error message
        write( error_unit,"(A,/,A)" )           'ERROR',&
                                                ' |-code: '//clean_str(error_code_str)
        if ( separate ) then
            write( error_unit,"(A)" )           ' |-message: '//err_message(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(A,/)")   '            '//err_message(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(A)" )       '            '//err_message(i*set_len+1:(i+1)*set_len)
            enddo
        else !if ( .not.separate ) then
            write( error_unit,"(A,/)" )         ' |-message: '//clean_str(err_message)
        end if
    
        if ( l_terminate ) stop                 'Program terminated.'
    end if
    
    end subroutine jc_error_out
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine firsttime_error_out                                      !
    !-------------------------------------------------------------------+
    
    use common_data,    only: api_version,file_version,dir,filename
    use functions,      only: clean_str
    
    implicit none
    
    integer             :: unit,istat
    logical             :: alive
    
    write( error_unit,"(A)" )   ''
    write( error_unit,"(A)" )   'Current API Version is: '//api_version%string
    write( error_unit,"(A)" )   '   file API Version is: '//file_version%string
    write( error_unit,"(A)" )   ''
    write( error_unit,"(A,I2)" )'             log level:',log_level
    write( error_unit,"(A)" )   '              log file: "'//log_file//'"'
    write( error_unit,"(A)" )   ''
    write( error_unit,"(A)" )   '     json control file: "'//clean_str(dir//filename)//'"'
    
    if ( log_level>0 ) then
        
        ! output to log file
        inquire( file=log_file,exist=alive )
        if ( alive ) then
            open (  NEWUNIT=unit,FILE=log_file,STATUS='OLD',POSITION='APPEND',IOSTAT=istat )
        else !if ( .not.alive ) then
            open (  NEWUNIT=unit,FILE=log_file,STATUS='NEW',POSITION='APPEND',IOSTAT=istat )
        end if
        
        write( unit,"(A)" )     '============================================================'
        write( unit,"(A)" )     time_RFC3339()
        write( unit,"(A)" )     'Current API Version is: '//api_version%string
        write( unit,"(A)" )     '   file API Version is: '//file_version%string
    
        select case ( log_level )   ! 1="error" | 2="warning" | 3="info" | 4 ="debug"
        case ( 1 )
            write( unit,"(A)" ) '             log level: ERROR'
        case ( 2 )
            write( unit,"(A)" ) '             log level: WARNING'
        case ( 3 )
            write( unit,"(A)" ) '             log level: INFO'
        case ( 4 )
            write( unit,"(A)" ) '             log level: DEBUG'
        end select
    
        write( unit,"(A)" )     '              log file: "'//log_file//'"'
        write( unit,"(A)" )     '     json control file: "'//clean_str(dir//filename)//'"'
        write( unit,"(A)" )     '------------------------------------------------------------'
        
        if ( istat==0 ) close( unit=unit, iostat=istat )
        
    end if
    
    end subroutine firsttime_error_out
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    function time_RFC3339()                                             !
    !-------------------------------------------------------------------+
    
    implicit none
    
    character(:), allocatable   :: time_RFC3339
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    
    call date_and_time(DATE=date,TIME=time,ZONE=zone)
    time_RFC3339 = date(1:4)//'-'//date(5:6)//'-'//date(7:8)&
                    //'T'//time(1:2)//':'//time(3:4)//':'//time(5:10)&
                    //zone(1:3)//':'//zone(4:5)
    
    end function time_RFC3339
    
    !====================================================================
    
    
end module jc_error_out_mod
