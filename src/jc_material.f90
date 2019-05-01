!*****************************************************************************************
!> author: nescirem
!  date: 5/01/2019
!
!  Module parse base materials info.
!

module jc_material_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code
    use functions,                      only: clean_str
    use check_uniqueness_mod
    
    implicit none

    private
    public :: jc_material

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_material                                              !
    !-------------------------------------------------------------------+
    
    use common_data,            only: n_material,MTR

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p,p_temp     !! a pointer for low-level manipulations
    
    integer                     :: i            !! counter
    character(len=16)           :: i_str        !! counter in string
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found,is_unique
    
    call json%initialize()
    
    ! parse the json file
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    else
        call core%initialize()
        if ( core%failed() ) call core%print_error_message( error_unit )
    end if
    call progress_out
    
    ! get num of the material
    error_code = error_code+1
    call json%get( 'material', p )
    if ( json%failed() ) call json%print_error_message( error_unit )
    call core%info( p, var_type=var_type, n_children=n_material )
    if ( n_material==0 ) call error_out( 'Material property is required, please check: material' )
    allocate ( MTR(n_material) ) 
    call progress_out
    
    ! parase the id of each material
    error_code = error_code+1
    do i=1,n_material
        write( i_str,* ) i
        call core%get( p,'@('//clean_str(i_str)//')',p_temp )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_temp, name=MTR(i)%material_id )
    end do
    call progress_out
    
    ! get the material base information
    error_code = error_code+1
    do i=1,n_material
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.name', str_temp, found )
        if ( found ) MTR(i)%material_name = str_temp
        
        call json%get( 'material.'//clean_str(MTR(i)%material_id)//'.rho', MTR(i)%rho, found )
        if ( .not.found ) call error_out( 'Must specify the material density,'&
                            //' please check: material.'//clean_str(MTR(i)%material_id)//'.rho' )
        
    end do
    call progress_out
    
    ! clean up
    call core%destroy()
    if ( core%failed() ) call core%print_error_message( error_unit )
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_material
    
    !====================================================================
    

end module jc_material_mod