!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse asi solver settings.
!

module jc_asi_solver_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    use output_mod
    
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
    
    character(kind=CK,len=:),allocatable        :: str_temp
    character(:),allocatable                    :: path
    logical                                     :: found
    integer                                     :: i
    character(len=16)                           :: i_str
    
    
    call info_out( 'Parse ASI solver settings' )
    
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    call debug_out( 'Allocate ASI solver settings in each zone' )
    allocate ( have_mean_flow(n_zone),is_porous(n_zone),wave_flag(n_zone),n_wave(n_zone) )
    
    ! parse ASI settings
    error_code = error_code+1
    do i=1, n_zone
        
        write( i_str,* ) i
        path = 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.problem'
        call debug_out( 'Get:', path )
        call json%get( path, str_temp, found )
        if ( .not.found ) call error_out( 'Must specify problem type, please check: '//path, exit_if_error )
        select case ( str_temp )
        case ( 'fluid' )
            call error_out( 'In ASI solver, zone type should not be "fluid", '&
                        //' please check: '//path, exit_if_error )
        case ( 'solid' )
            call error_out( 'In ASI solver, zone type should not be "solid", '&
                        //' please check: '//path, exit_if_error )
        case ( 'vibration' )
            zone_type(i) = 3
            call debug_out( 'The zone problem type is "vibration"' )
        case ( 'acoustic' )
            zone_type(i) = 6
            call debug_out( 'The zone problem type is "acoustic"' )
        case default
            call error_out( 'Unknown zone type "'//str_temp//'", please check: '//path, exit_if_error )
        end select
        
        error_code = error_code+1
        path = 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.waveFlag'
        call debug_out( 'Get:', path )
        call json%get( path, wave_flag(i), found )
        if ( .not.found ) call error_out( 'Must specify wave flag, please check: '//path, exit_if_error )
        call debug_out( 'waveFlag=',wave_flag(i) )
        
        error_code = error_code+1
        path = 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.numWave'
        call debug_out( 'Get:', path )
        call json%get( path, n_wave(i), found )
        if ( .not.found ) call error_out( 'Must specify number of waves, please check: '//path, exit_if_error )
        call debug_out( 'n_wave=',n_wave(i) )
        
        path = 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.haveMeanFlow'
        call debug_out( 'Get:', path )
        call json%get( path, have_mean_flow(i), found )
        if ( found ) then
            call debug_out( 'have_mean_flow=',have_mean_flow(i) )
        else !if ( .not.found ) then
            have_mean_flow(i) = .false. !default no mean flow
            call warning_out( 'Haven''t define '//path//', default FALSE' )
        end if
        
        
        
        path = 'solver.asiSetting.zone.'//clean_str(zone_id(i))//'.isPorous'
        call debug_out( 'Get:', path )
        call json%get( path, is_porous(i), found )
        if ( found ) then
            call debug_out( 'is_porous=',is_porous(i) )
        else !if ( .not.found ) then
            is_porous(i) = .false. !default not porous media
            call warning_out( 'Haven''t define '//path//', default FALSE' )
        end if
        
    end do
        
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_asi_solver
    
    !====================================================================
    
    
end module jc_asi_solver_mod
