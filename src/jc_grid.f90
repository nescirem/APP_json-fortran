!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module parse grid info.

module jc_grid_control_mod

    use json_module, CK => json_CK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: dir,filename,error_code,&
                                            grid_file,grid_type
    use functions,                      only: clean_str
    use check_uniqueness_mod
                                        
    implicit none

    private
    public :: jc_grid_control

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_grid_control                                          !
    !-------------------------------------------------------------------+
    
    use common_data,    only: n_grid

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    integer(IK)                 :: var_type
    
    call json%initialize()
    
    ! parse the json file
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    else ! print the parsed data to the console
        !write( error_unit,'(A)' ) ''
        !write( error_unit,'(A)') 'printing the file...'
        !call json%print_file( int(error_unit,IK) )
        !if ( json%failed() ) then
        !    call json%print_error_message( error_unit )
        !end if
        call core%initialize()
        if ( core%failed() ) call core%print_error_message( error_unit )
    end if
    call progress_out
    
    ! get num of grid files
    call json%get( 'grid', p )
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    call core%info( p, var_type=var_type, n_children=n_grid )
    error_code = error_code+1
    select case ( var_type )
    case ( json_object )
        n_grid = 1
        allocate ( grid_file(n_grid),grid_type(n_grid) )
        call parse_grid( p )
    case ( json_array )
        allocate ( grid_file(n_grid),grid_type(n_grid) )
        call parse_grids( p )
    case default
        call error_out( 'Unknown structure, please check: grid' )
    end select
    call progress_out
    
    ! clean up
    call core%destroy()
    if ( core%failed() ) call core%print_error_message( error_unit )
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_grid_control
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine parse_grid ( p_local )                                   !
    !-------------------------------------------------------------------+
    
    use common_data,    only: grid_file,grid_type,&
                            n_zone,zone_name,zone_type,zone_id
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p_local
    type(json_value),pointer            :: p_lower_1, p_temp
    type(json_core)                     :: core
    
    integer                     :: i            !! counter
    character(len=16)           :: i_str
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found,is_unique
    
    call core%initialize()
    
    ! get grid file name
    error_code = error_code+1
    call core%get( p_local,'fileName', str_temp, found )
    if ( core%failed() ) call core%print_error_message( error_unit )
    if ( found ) then
        write( grid_file(1),* ) str_temp
    else
        call error_out( 'Must specify grid file name, please check: grid.fileName' )
    end if
    call progress_out
    
    ! get grid file type
    error_code = error_code+1
    call core%get( p_local,'fileType', str_temp, found )
    if ( core%failed() ) call core%print_error_message( error_unit )
    if ( found ) then
        select case (str_temp)
        case ( 'msh' )
            grid_type(1) = 1
        case ( 'cgns' )
            grid_type(1) = 2
        case default
            call error_out( 'Unknown grid file type "'//str_temp//'", please check: grid.fileType' )
        end select
    else
        grid_type(1) = 2 ! default cgns
    endif
    call progress_out
    
    ! get num of the elements
    call core%get( p_local,'zone', p_lower_1 )
    if ( core%failed() ) call core%print_error_message( error_unit )
    call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
    ! allocate zone data
    allocate( zone_id(n_zone), zone_name(n_zone),zone_type(n_zone) )
    call progress_out
    
    ! parase the name/id of each elements
    do i=1,n_zone
        write( i_str,* ) i
        call core%get( p_lower_1,'@('//clean_str(i_str)//')',p_temp )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_temp, name=str_temp )
        write( zone_id(i),'(A)' ) str_temp
    end do
    call progress_out
    
    ! check the uniqueness of zones id 
    error_code = error_code+1
    call check_uniqueness( zone_id,size(zone_id),is_unique )
    if ( .not.is_unique ) call error_out( 'Zone ID must be unique.' )
    call progress_out
    
    ! parase zone settings
    do i=1,n_zone
        call core%get( p_lower_1, clean_str(zone_id(i))//'.name', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) write( zone_name(i),* ) str_temp
    end do
    call progress_out
    
    ! clean up
    call core%destroy()
    if ( core%failed() ) call core%print_error_message( error_unit )
    
    end subroutine parse_grid
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine parse_grids ( p_local )                                  !
    !-------------------------------------------------------------------+
    
    use common_data,    only: n_grid,grid_file,grid_type,&
                            n_zone,zone_name,zone_type,zone_id,zone_material_id
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p_local
    type(json_value),pointer            :: p_lower_1, p_temp
    type(json_core)                     :: core
    
    integer                     :: i,i_grid            !! counter
    character(len=16)           :: i_str,i_zone_str
    integer                     :: i_zone = 0
    integer                     :: np_zone = 0
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found,is_unique
    
    call core%initialize()
    
    ! get grid files' name and their type
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        error_code = error_code+1
        call core%get( p_local,'@['//clean_str(i_str)//'].fileName', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) then
            write( grid_file(i_grid),* ) str_temp
        else
            call error_out( 'Must specify grid file name, please check: grid.fileName' )
        end if
        call progress_out
        
        error_code = error_code+1
        call core%get( p_local,'@['//clean_str(i_str)//'].fileType', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) then
            select case (str_temp)
            case ( 'msh' )
                grid_type(i_grid) = 1
            case ( 'cgns' )
                grid_type(i_grid) = 2
            case default
                call error_out( 'Unknown grid file type "'//str_temp//'", please check: grid.fileType' )
            end select
        else
            grid_type(i_grid) = 2 ! default cgns
        end if
        call progress_out
    
        ! get num of the elements
        call core%get( p_local,'@['//clean_str(i_str)//'].zone', p_lower_1 )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        np_zone = np_zone+n_zone
    enddo
    ! allocate zone data
    allocate( zone_id(np_zone),zone_name(np_zone),zone_type(np_zone),zone_material_id(np_zone) )
    call progress_out
    
    ! parase the name/id of each elements
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        call core%get( p_local,'@['//clean_str(i_str)//'].zone', p_lower_1 )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        do i=1,n_zone
            i_zone = i_zone+1
            write( i_zone_str,* ) i
            call core%get( p_lower_1,'@('//clean_str(i_zone_str)//')',p_temp )
            if ( core%failed() ) call core%print_error_message( error_unit )
            call core%info( p_temp, name=str_temp )
            write( zone_id(i_zone),'(A)' ) str_temp
        end do
    end do
    call progress_out
    
    ! check the uniqueness of zones id 
    error_code = error_code+1
    call check_uniqueness( zone_id,size(zone_id),is_unique )
    if ( .not.is_unique ) call error_out( 'Zone ID must be unique.' )
    call progress_out
    
    ! parase zone name
    i_zone = 0
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        call core%get( p_local,'@['//clean_str(i_str)//'].zone', p_lower_1 )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        
        do i=1,n_zone
            i_zone = i_zone+1
            call core%get( p_lower_1, clean_str(zone_id(i_zone))//'.name', str_temp, found )
            if ( core%failed() ) call core%print_error_message( error_unit )
            if ( found ) write( zone_name(i_zone),* ) str_temp
        end do
    end do
    call progress_out
    
    n_zone = np_zone
    
    ! clean up
    call core%destroy()
    if ( core%failed() ) call core%print_error_message( error_unit )

    end subroutine parse_grids
    
    !====================================================================
    

end module jc_grid_control_mod
