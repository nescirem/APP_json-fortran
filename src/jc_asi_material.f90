!*****************************************************************************************
!> author: nescirem
!  date: 5/01/2019
!
!  Module parse ASI materials info.
!

module jc_asi_material_mod

    use json_module, IK => json_IK, RK => json_RK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code
    use functions,                      only: clean_str
    use check_uniqueness_mod
    
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
    character(len=16)           :: i_str        !! counter in string
    integer(IK)                 :: var_type
    real(kind=RK)                               :: real_temp
    logical                                     :: found,is_unique
    
    call json%initialize()
    
    ! parse the json file
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    end if
    call progress_out
    
    ! get the material ASI information
    error_code = error_code+1
    do i=1,n_material
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.acousticVelocity', real_temp, found )
        if ( .not.found ) call error_out( 'Must specify acoustic velocity, please check: '&
                                        //'material.'//clean_str(MTR(i)%material_id)//'.acousticVelocity' )
        MTR(i)%acoustic_velocity = real_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.heatCapacityRatio', real_temp, found )
        if ( found ) MTR(i)%heat_capacity_ratio = real_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.structureConstant', real_temp, found )
        if ( found ) MTR(i)%structure_constant = real_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.flowResistance', real_temp, found )
        if ( found ) MTR(i)%flow_resistance = real_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.porousity', real_temp, found )
        if ( found ) MTR(i)%porousity = real_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.ambientPressure', real_temp, found )
        if ( found ) MTR(i)%ambient_pressure = real_temp
        
    end do
    call progress_out
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_asi_material
    
    !====================================================================
    

end module jc_asi_material_mod
