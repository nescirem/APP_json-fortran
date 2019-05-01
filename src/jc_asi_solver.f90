!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse base solver settings.
!

module jc_asi_solver_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    
    implicit none

    private
    public :: jc_asi_solver

contains    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_asi_solver                                            !
    !-------------------------------------------------------------------+
    
    use common_data,            only: zone_type,have_mean_flow,is_porous,&
                                    wave_flag,n_wave
    
    implicit none
    
    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    integer(IK)                                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    integer                                     :: i
    character(len=16)                           :: i_str
    
    call json%initialize()
    
    ! parse the json file
    error_code = error_code+1
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    end if
    call progress_out
    
    allocate ( have_mean_flow(n_zone),is_porous(n_zone),wave_flag(n_zone),n_wave(n_zone) )
    
    ! parse ASI settings
    error_code = error_code+1
    do i=1, n_zone
        write( i_str,* ) i
        call json%get( 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.problem', str_temp, found )
        if ( .not.found ) call error_out( 'Must specify problem type, please check: '&
                                        //'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.problem' )
        select case ( str_temp )
        case ( 'fluid' )
            call error_out( 'In ASI solver, zone type should not be "fluid", '&
                        //' please check: solver.asiSetting.zone.'//clean_str(zone_id(i))//'.type' )
        case ( 'solid' )
            call error_out( 'In ASI solver, zone type should not be "solid", '&
                        //' please check: solver.asiSetting.zone.'//clean_str(zone_id(i))//'.type' )
        case ( 'vibration' )
            zone_type(i) = 3
        case ( 'acoustic' )
            zone_type(i) = 6
        case default
            call error_out( 'Unknown zone type "'//str_temp//'",'&
                    //' please check: solver.asiSetting.zone.'//clean_str(zone_id(i))//'.type' )
        end select
        call progress_out
        
        error_code = error_code+1
        call json%get( 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.waveFlag', wave_flag(i), found )
        if ( .not.found ) call error_out( 'Must specify wave flag, please check: '&
                                        //'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.waveFlag' )
        call progress_out
        
        error_code = error_code+1
        call json%get( 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.numWave', n_wave(i), found )
        if ( .not.found ) call error_out( 'Must specify number of waves, please check: '&
                                        //'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.numWave' )
        call progress_out
        
        call json%get( 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.haveMeanFlow', have_mean_flow(i), found )
        if ( .not.found ) have_mean_flow(i) = .false. !default no mean flow
        
        call json%get( 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.isPorous', is_porous(i), found )
        if ( .not.found ) is_porous(i) = .false. !default not porous media
        
    end do
    call progress_out
        
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_asi_solver
    
    !====================================================================
    
    
end module jc_asi_solver_mod
