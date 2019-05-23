!*****************************************************************************************
!> author: nescirem
!  date: 5/01/2019
!
!  Module parse ASI materials info.
!

module jc_asi_material_mod

    use json_module, IK => json_IK, RK => json_RK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code
    use functions,                      only: clean_str
    use check_uniqueness_mod
    use output_mod
    
    implicit none

    private
    public :: jc_asi_material

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_asi_material                                          !
    !-------------------------------------------------------------------+
    
    use common_data,            only: n_material,MTR

    implicit none
    
    type(json_file)             :: json         !! the JSON structure read from the file
    
    integer                     :: i            !! counter
    real(kind=RK)               :: real_temp
    character(:),allocatable    :: str_temp
    logical                     :: found
    
    
    call info_out( 'Parse ASI materials info' )
    
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    ! get the material ASI information
    error_code = error_code+1
    do i=1,n_material
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.acousticVelocity'
        call debug_out( 'Get:', str_temp )
        call json%get( str_temp, real_temp, found )
        if ( .not.found ) call error_out( 'Must specify acoustic velocity, please check: '&
                                        //str_temp, exit_if_error )
        MTR(i)%acoustic_velocity = real_temp
        call debug_out( str_temp//'=',MTR(i)%acoustic_velocity )
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.heatCapacityRatio'
        call debug_out( 'Get:', str_temp )
        call json%get( str_temp, real_temp, found )
        if ( found ) then
            MTR(i)%heat_capacity_ratio = real_temp
            call debug_out( str_temp//'=',MTR(i)%heat_capacity_ratio )
        else !if ( .not.found ) then
            MTR(i)%heat_capacity_ratio = 1.4d0
            call warning_out( 'Haven''t define '//str_temp//', default air 1.4' )
        end if
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.structureConstant'
        call json%get( str_temp, real_temp, found )
        if ( found ) then
            MTR(i)%structure_constant = real_temp
            call debug_out( str_temp//'=',MTR(i)%structure_constant )
        else !if ( .not.found ) then
            MTR(i)%structure_constant = 1.0d0
            call warning_out( 'Haven''t define '//str_temp//', default 1.0' )
        end if
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.flowResistance'
        call json%get( str_temp, real_temp, found )
        if ( found ) then
            MTR(i)%flow_resistance = real_temp
            call debug_out( str_temp//'=',MTR(i)%flow_resistance )
        else !if ( .not.found ) then
            MTR(i)%flow_resistance = 0.0d0
            call warning_out( 'Haven''t define '//str_temp//', default 0.0d0 (Pa∙s∙m-2)' )
        end if
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.porousity'
        call json%get( str_temp, real_temp, found )
        if ( found ) then
            MTR(i)%porousity = real_temp
            call debug_out( str_temp//'=',MTR(i)%porousity )
        else !if ( .not.found ) then
            MTR(i)%porousity = 0.0d0
            call warning_out( 'Haven''t define '//str_temp//', default 0.0d0' )
        end if
        
        str_temp = 'material.'//clean_str(MTR(i)%material_id)//'.ambientPressure'
        call json%get( str_temp, real_temp, found )
        if ( found ) then
            MTR(i)%ambient_pressure = real_temp
            call debug_out( str_temp//'=',MTR(i)%ambient_pressure )
        else !if ( .not.found ) then
            MTR(i)%ambient_pressure = 1.01325d5
            call warning_out( 'Haven''t define '//str_temp//', default 101325 (Pa)' )
        end if
        
    end do
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_asi_material
    
    !====================================================================
    

end module jc_asi_material_mod
