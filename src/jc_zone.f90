!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse zones information.
!

module jc_zone_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    
    implicit none

    private
    public :: jc_zone

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_zone                                                  !
    !-------------------------------------------------------------------+
    
    use common_data,    only: n_phase,zone_material_id,is_homogeneous

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    integer(IK)                                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    integer                                     :: i
    character(len=16)                           :: i_str
    
    
    call json%initialize()
    
    allocate ( zone_material_id(n_zone),is_homogeneous(n_zone) )
    
    ! parse the json file
    error_code = error_code+1
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file',terminate=.true. )
    end if
    call progress_out
    
    error_code = error_code+1
    do i=1,n_zone
        call json%get( 'zone.'//clean_str(zone_id(i))//'.material', str_temp, found )
        if ( found ) then
            write( zone_material_id(i),* ) str_temp
        else !if ( .not.found ) then
            call error_out( 'Must specify zone material, please check: zone.'&
                    //clean_str(zone_id(i))//'.material',exit_if_error )
        endif
        
        call json%get( 'zone.'//clean_str(zone_id(i))//'.isHomogeneous',is_homogeneous(i), found )
        if ( .not.found ) is_homogeneous(i) = .true. ! default homogeneous
    enddo
    call progress_out
        
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_zone
    
    !====================================================================
    
    
end module jc_zone_mod
