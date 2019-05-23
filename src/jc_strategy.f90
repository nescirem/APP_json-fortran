!*****************************************************************************************
!> author: nescirem
!  date: 5/01/2019
!
!  Module parse discrete strategy settings.
!

module jc_strategy_mod

    use json_module, CK => json_CK, RK => json_RK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    use check_uniqueness_mod
    use output_mod
    
    implicit none

    private
    public :: jc_strategy

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_strategy                                              !
    !-------------------------------------------------------------------+
    
    use common_data,            only: time_stategy,time_start,time_end,&
                                    transient_formulation,delta_T,gradient
    
    implicit none
    
    type(json_file)             :: json         !! the JSON structure read from the file
    
    integer                     :: i            !! counter
    character(len=16)           :: i_str
    real(kind=RK)               :: real_temp
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    logical,pointer,dimension(:)                :: is_tf_setted,is_gs_setted,is_dt_setted
    
    
    call info_out( 'Parse strategy settings, '&
                //'If you provide both global variables and local zone variables,'&
                //' the program will give priority to local variables' )
    
    allocate ( is_tf_setted(n_zone),is_gs_setted(n_zone),is_dt_setted(n_zone) )
    
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    ! allocate strategy settings
    call info_out( 'Allocate memeory for transient_formulation, delta_T, and gradient for each zone' )
    allocate ( transient_formulation(n_zone),delta_T(n_zone),gradient(n_zone) )
    
    ! get the time discrete strategy
    error_code = error_code+1
    call debug_out( 'Get:','strategy.time' )
    call json%get( 'strategy.time', str_temp, found )
    if ( .not.found ) call error_out( 'Must specify the time stategy, please check: strategy.time',exit_if_error )
    select case ( str_temp )
    case ( 'transient' )
        time_stategy = 1
        call debug_out( 'The time_stategy is set to "1=transient"' )
    case ( 'steady' )
        time_stategy = 2
        call debug_out( 'The time_stategy is set to "2=steady"' )
    case default
        call error_out( 'Unknown time discrete stategy "'//str_temp//'", please check: strategy.time',exit_if_error )
    end select
    
    ! get the global settings of the time discrete strategy
    is_tf_setted(:) = .false.
    is_dt_setted(:) = .false.
    if ( time_stategy==1 ) then
        
        error_code = error_code+1
        call debug_out( 'Get:','strategy.timeSetting.formulation' )
        call json%get( 'strategy.timeSetting.formulation', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'explicit' )
                transient_formulation(:) = 1
                call debug_out( 'The transient_formulation of all zones is','1=explicit' )
            case ( 'implicit' )
                transient_formulation(:) = 2
                call debug_out( 'The transient_formulation of all zones is','2=implicit' )
            case default
                call error_out( 'Unknown time discrete stategy "'//str_temp//'", '&
                        //'please check: strategy.timeSetting.formulation',exit_if_error )
            end select
            is_tf_setted(:) = .true.
        end if
        
        error_code = error_code+1
        call debug_out( 'Get:','strategy.timeSetting.timeStart' )
        call json%get( 'strategy.timeSetting.timeStart', real_temp, found )
        if ( .not.found ) call error_out( 'Must specify the start time, '&
                                //'please check: strategy.timeSetting.timeStart',exit_if_error )
        time_start = real_temp
        call debug_out( 'time_start=',time_start,'(s)' )
        
        
        error_code = error_code+1
        call debug_out( 'Get:','strategy.timeSetting.timeEnd' )
        call json%get( 'strategy.timeSetting.timeEnd', real_temp, found )
        if ( .not.found ) call error_out( 'Must specify the start time, '&
                                //'please check: strategy.timeSetting.timeEnd',exit_if_error )
        time_end = real_temp
        call debug_out( 'time_end=',time_end,'(s)' )
        
        call debug_out( 'Get:','strategy.timeSetting.deltaT' )
        call json%get( 'strategy.timeSetting.deltaT', real_temp, found )
        if ( found ) then
            delta_T(:) = real_temp
            call debug_out( 'delta_T=',real_temp,'(s)' )
            is_dt_setted(:) = .true.
        end if
        
    !elseif ( time_stategy/=1 ) then
    
    end if
    
    ! get the global settings of the gardient solution strategy
    error_code = error_code+1
    is_gs_setted(:) = .false.
    call debug_out( 'Get:','strategy.gradient' )
    call json%get( 'strategy.gradient', str_temp, found )
    if ( found ) then
        select case ( str_temp )
        case ( 'linear' )
            gradient(:) = 1
            call debug_out( 'The gradient strategy of all zones is','1=linear' )
        case ( 'least squares' )
            gradient(:) = 2
            call debug_out( 'The gradient strategy of all zones is','2=least squares' )
        case default
            call error_out( 'Unknown gradient solution strategy "'//str_temp &
                    //'", please check: strategy.gradient',exit_if_error )
        end select
        is_gs_setted(:) = .true.
    else !if ( .not.found ) then
        call debug_out( 'Not found:','strategy.gradient' )
    end if
    
    ! get the discrete strategy settings of each zone 
    error_code = error_code+1
    do i=1,n_zone
        
        write( i_str,* ) i
        call debug_out( 'Get:','strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation' )
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'explicit' )
                transient_formulation(i) = 1
                call debug_out( 'The transient_formulation of zones['//clean_str(i_str)//'] is','1=explicit' )
            case ( 'implicit' )
                transient_formulation(i) = 2
                call debug_out( 'The transient_formulation of zones['//clean_str(i_str)//'] is','2=implicit' )
            case default
                call error_out( 'Unknown time discrete stategy "'//str_temp//'", '&
                    //'please check: strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation',exit_if_error )
            end select
            is_tf_setted(i) = .true.
        else !if ( .not.found ) then
            call debug_out( 'Not found:','strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation' )
        end if
        
        call debug_out( 'Get:','strategy.zone.'//clean_str(zone_id(i))//'.gradient' )
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.gradient', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'linear' )
                gradient(i) = 1
                call debug_out( 'The gradient strategy of zone['//clean_str(i_str)//'] is','1=linear' )
            case ( 'least squares' )
                gradient(i) = 2
                call debug_out( 'The gradient strategy of zone['//clean_str(i_str)//'] is','2=least squares' )
            case default
                call error_out( 'Unknown gradient solution strategy "'//str_temp//'", '&
                        //'please check: strategy.zone.'//clean_str(zone_id(i))//'.gradient',exit_if_error )
            end select
            is_gs_setted(i) = .true.
        else !if ( .not.found ) then
            call debug_out( 'Not found:','strategy.zone.'//clean_str(zone_id(i))//'.gradient' )
        end if
        
        call debug_out( 'Get:','strategy.zone.'//clean_str(zone_id(i))//'.deltaT' )
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.deltaT', real_temp, found )
        if ( found ) then
            delta_T(i) = real_temp
            call debug_out( 'delta_T=',real_temp,'(s)' )
            is_dt_setted(i) = .true.
        else !if ( .not.found ) then
            call debug_out( 'Not found:','strategy.zone.'//clean_str(zone_id(i))//'.deltaT' )
        end if
        
    end do
    
    ! check if there missing settings in some zone 
    error_code = error_code+1
    if ( .not.all( is_dt_setted ) ) call error_out( 'Must set deltaT for each zone',exit_if_error )
    if ( .not.all( is_gs_setted ) ) call error_out( 'Must set gardient strategy for each zone',exit_if_error )
    if ( .not.all( is_tf_setted ) ) call error_out( 'Must set time formulation strategy for each zone',exit_if_error )
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_strategy
    
    !====================================================================
    
    
end module jc_strategy_mod
