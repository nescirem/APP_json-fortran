!*****************************************************************************************
!> author: nescirem
!  date: 5/01/2019
!
!  Module parse discrete strategy settings.
!

module jc_strategy_mod

    use json_module, CK => json_CK, RK => json_RK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    use check_uniqueness_mod
    
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
    character(len=16)           :: i_str        !! counter in string
    real(kind=RK)               :: real_temp
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    logical,pointer,dimension(:)                :: is_tf_setted,is_gs_setted,is_dt_setted
    
    
    allocate ( is_tf_setted(n_zone),is_gs_setted(n_zone),is_dt_setted(n_zone) )
    
    call json%initialize()
    
    ! parse the json file
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    end if
    call progress_out
    
    ! allocate strategy settings
    allocate ( transient_formulation(n_zone),delta_T(n_zone),gradient(n_zone) )
    call progress_out
    
    ! get the time discrete strategy
    error_code = error_code+1
    call json%get( 'strategy.time', str_temp, found )
    if ( .not.found ) call error_out( 'Must specify the time stategy, please check: strategy.time' )
    select case ( str_temp )
    case ( 'transient' )
        time_stategy = 1
    case ( 'steady' )
        time_stategy = 2
    case default
        call error_out( 'Unknown time discrete stategy "'//str_temp//'", please check: strategy.time' )
    end select
    call progress_out
    
    ! get the global settings of the time discrete strategy
    is_tf_setted(:) = .false.
    is_dt_setted(:) = .false.
    if ( time_stategy==1 ) then
        
        error_code = error_code+1
        call json%get( 'strategy.timeSetting.formulation', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'explicit' )
                transient_formulation(:) = 1
            case ( 'implicit' )
                transient_formulation(:) = 2
            case default
                call error_out( 'Unknown time discrete stategy "'//str_temp//'", '&
                        //'please check: strategy.timeSetting.formulation' )
            end select
            is_tf_setted(:) = .true.
        end if
        call progress_out
        
        error_code = error_code+1
        call json%get( 'strategy.timeSetting.timeStart', real_temp, found )
        if ( .not.found ) call error_out( 'Must specify the start time, '&
                                //'please check: strategy.timeSetting.timeStart' )
        time_start = real_temp
        call progress_out
        
        error_code = error_code+1
        call json%get( 'strategy.timeSetting.timeEnd', real_temp, found )
        if ( .not.found ) call error_out( 'Must specify the start time, '&
                                //'please check: strategy.timeSetting.timeEnd' )
        time_end = real_temp
        call progress_out
        
        call json%get( 'strategy.timeSetting.deltaT', real_temp, found )
        if ( found ) then
            delta_T(:) = real_temp
            is_dt_setted(:) = .true.
        end if
        
    endif
    call progress_out
    
    ! get the global settings of the gardient solution strategy
    error_code = error_code+1
    is_gs_setted(:) = .false.
    call json%get( 'strategy.gradient', str_temp, found )
    if ( found ) then
        select case ( str_temp )
        case ( 'linear' )
            gradient(:) = 1
        case ( 'least squares' )
            gradient(:) = 2
        case default
            call error_out( 'Unknown gradient solution strategy "'//str_temp//'", please check: strategy.gradient' )
        end select
        is_gs_setted(:) = .true.
    end if
    call progress_out
    
    ! get the discrete strategy settings of each zone 
    error_code = error_code+1
    do i=1,n_zone
        
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'explicit' )
                transient_formulation(i) = 1
            case ( 'implicit' )
                transient_formulation(i) = 2
            case default
                call error_out( 'Unknown time discrete stategy "'//str_temp//'", '&
                    //'please check: strategy.zone.'//clean_str(zone_id(i))//'.timeFormulation' )
            end select
            is_tf_setted(i) = .true.
        end if
        
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.gradient', str_temp, found )
        if ( found ) then
            select case ( str_temp )
            case ( 'linear' )
                gradient(i) = 1
            case ( 'least squares' )
                gradient(i) = 2
            case default
                call error_out( 'Unknown gradient solution strategy "'//str_temp//'", '&
                        //'please check: strategy.zone.'//clean_str(zone_id(i))//'.gradient' )
            end select
            is_gs_setted(i) = .true.
        end if
        
        call json%get( 'strategy.zone.'//clean_str(zone_id(i))//'.deltaT', real_temp, found )
        if ( found ) then
            delta_T(i) = real_temp
            is_dt_setted(i) = .true.
        end if
        
    end do
    call progress_out
    
    ! check if there missing settings in some zone 
    error_code = error_code+1
    if ( any( is_dt_setted==.false. ) ) call error_out( 'Must set deltaT for each zone' )
    if ( any( is_gs_setted==.false. ) ) call error_out( 'Must set gardient strategy for each zone' )
    if ( any( is_tf_setted==.false. ) ) call error_out( 'Must set time formulation strategy for each zone' )
    call progress_out
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_strategy
    
    !====================================================================
    
    
end module jc_strategy_mod
