!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module parse grid info.
!

module jc_grid_mod

    use json_module, CK => json_CK, IK => json_IK!, LK => json_LK, RK => json_RK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            grid_file,grid_type
    use functions,                      only: clean_str
    use check_uniqueness_mod
    use output_mod
                                        
    implicit none

    private
    public :: jc_grid

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_grid                                                  !
    !-------------------------------------------------------------------+
    
    use common_data,            only: n_grid

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    integer(IK)                 :: var_type
    character(len=32)           :: str_temp
    
    call info_out( 'Parse grid info' )
    
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    call core%initialize()
    
    ! get num of grid files
    call debug_out( 'Get: "grid" ' )
    call json%get( 'grid', p )
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    call info_out( 'Get num of grid files' )
    error_code = error_code+1
    call core%info( p, var_type=var_type, n_children=n_grid )
    if ( n_grid==0 ) call error_out( 'Grid information is required, please check: grid',exit_if_error )
    call progress_out
    
    error_code = error_code+1
    call debug_out( 'Check value type in "grid" if it is object or array' )
    select case ( var_type )
    case ( json_object )
        n_grid = 1
        call debug_out( 'The value type in "grid" is object, allocate only one grid file' )
        allocate ( grid_file(n_grid),grid_type(n_grid) )
        call parse_grid( p )
    case ( json_array )
        write (str_temp,*) n_grid
        call debug_out( 'The value type in "grid" is array, allocate '//clean_str(str_temp)//' grid file' )
        allocate ( grid_file(n_grid),grid_type(n_grid) )
        call parse_grids( p )
    case default
        call error_out( 'Unknown structure, please check: grid',exit_if_error )
    end select
    call progress_out
    
    ! clean up
    call core%destroy()
    call json%destroy()
    
    end subroutine jc_grid
    
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
    character(len=32)           :: i_temp_str
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found,is_unique
    
    
    call info_out( 'Parse grid file information' )
    call core%initialize()
    
    ! get grid file name
    call info_out( 'Get the grid file name' )
    error_code = error_code+1
    call core%get( p_local,'fileName', str_temp, found )
    if ( core%failed() ) call core%print_error_message( error_unit )
    if ( found ) then
        write( grid_file(1),* ) str_temp
        call debug_out( 'The grid file name is "'//clean_str(grid_file(1))//'"' )
    else
        call error_out( 'Must specify grid file name, please check: grid.fileName',exit_if_error )
    end if
    call progress_out
    
    ! get grid file type
    call info_out( 'Get the grid file type' )
    error_code = error_code+1
    call core%get( p_local,'fileType', str_temp, found )
    if ( core%failed() ) call core%print_error_message( error_unit )
    if ( found ) then
        select case (str_temp)
        case ( 'msh' )
            grid_type(1) = 1
            call debug_out( 'The grid file type is "msh"' )
        case ( 'cgns' )
            grid_type(1) = 2
            call debug_out( 'The grid file type is "cgns"' )
        case default
            call error_out( 'Unknown grid file type "'//str_temp//'", please check: grid.fileType',exit_if_error )
        end select
    else
        grid_type(1) = 2 ! default cgns
        call warning_out( 'Haven''t define "grid.fileType", the grid file type was set to default: "cgns"' )
    endif
    call progress_out
    
    ! get number of zone
    call info_out( 'Get the number of zone' )
    error_code = error_code+1
    call debug_out( 'Get: "grid.zone" ' )
    call core%get( p_local,'zone', p_lower_1 )
    if ( core%failed() ) call core%print_error_message( error_unit )
    call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
    if ( n_zone==0 ) call error_out( 'At least one zone in the grid file',exit_if_error )
    write (i_temp_str,*) n_zone
    call debug_out( 'The number of zone is '//clean_str(i_temp_str) )
    call progress_out
    
    ! allocate zone data
    call debug_out( 'Allocate the zone data' )
    allocate( zone_id(n_zone), zone_name(n_zone),zone_type(n_zone) )
    call progress_out
    
    ! parase the name/id of each elements
    call info_out( 'Parase the id of each zone' )
    do i=1,n_zone
        write( i_str,* ) i
        call core%get( p_lower_1,'@('//clean_str(i_str)//')',p_temp )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_temp, name=str_temp )
        write( zone_id(i),'(A)' ) str_temp
        call debug_out( 'Zone_id('//clean_str(i_str)//'): '//clean_str(zone_id(i)) )
    end do
    call progress_out
    
    ! check the uniqueness of zones id
    call info_out( 'Check the uniqueness of zones id' )
    error_code = error_code+1
    call check_uniqueness( zone_id,size(zone_id),is_unique )
    if ( .not.is_unique ) call error_out( 'Zone ID must be unique.',exit_if_error )
    call debug_out( 'Each zones ID in this grid file is unique' )
    call progress_out
    
    ! parase zone name
    call info_out( 'get the names of each zone' )
    do i=1,n_zone
        call core%get( p_lower_1, clean_str(zone_id(i))//'.name', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) write( zone_name(i),* ) str_temp
    end do
    call progress_out
    
    ! clean up
    call core%destroy()
    
    end subroutine parse_grid
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine parse_grids ( p_local )                                  !
    !-------------------------------------------------------------------+
    
    use common_data,    only: n_grid,grid_file,grid_type,&
                            n_zone,zone_name,zone_type,zone_id
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p_local
    type(json_value),pointer            :: p_lower_1, p_temp
    type(json_core)                     :: core
    
    integer                     :: i,i_grid            !! counter
    character(len=16)           :: i_str,i_zone_str
    character(len=32)           :: i_temp_str
    integer                     :: i_zone = 0
    integer                     :: np_zone = 0
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found,is_unique
    
    
    call info_out( 'Parse grid files information' )
    call core%initialize()
    
    ! get grid files' name and their type
    call info_out( 'Get grid files'' name and their type' )
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        error_code = error_code+1
        call core%get( p_local,'@['//clean_str(i_str)//'].fileName', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) then
            write( grid_file(i_grid),* ) str_temp
            call debug_out( 'The '//clean_str(i_str)//''' grid file name is "'//clean_str(grid_file(i_grid))//'"' )
        else
            call error_out( 'Must specify grid file name, please check: grid.fileName',exit_if_error )
        end if
        call progress_out
        
        error_code = error_code+1
        call core%get( p_local,'@['//clean_str(i_str)//'].fileType', str_temp, found )
        if ( core%failed() ) call core%print_error_message( error_unit )
        if ( found ) then
            select case (str_temp)
            case ( 'msh' )
                grid_type(i_grid) = 1
                call debug_out( 'The '//clean_str(i_str)//''' grid file type is "msh"' )
            case ( 'cgns' )
                grid_type(i_grid) = 2
                call debug_out( 'The '//clean_str(i_str)//''' grid file type is "cgns"' )
            case default
                call error_out( 'Unknown grid file type "'//str_temp&
                        //'", please check: grid.fileType',exit_if_error )
            end select
        else
            grid_type(i_grid) = 2 ! default cgns
            call warning_out( 'Haven''t define "grid['//clean_str(i_str)&
                    //'].fileType", the grid file type was set to default: "cgns"' )
        end if
        call progress_out
    
        ! get num of the zone
        call info_out( 'Get the number of zone' )
        error_code = error_code+1
        call core%get( p_local,'@['//clean_str(i_str)//'].zone', p_lower_1 )
        if ( core%failed() ) call core%print_error_message( error_unit )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        if ( n_zone==0 ) call error_out( 'At least one zone in each grid file',exit_if_error )
        call progress_out
        
        np_zone = np_zone+n_zone
        
    enddo
    
    write (i_temp_str,*) np_zone
    call debug_out( 'Total n_zone is '//clean_str(i_temp_str) )
    
    ! allocate zone data
    call debug_out( 'Allocate the zone data' )
    allocate( zone_id(np_zone),zone_name(np_zone),zone_type(np_zone) )
    call progress_out
    
    ! parase the name/id of each elements
    call info_out( 'Parase the id of each zone' )
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
            call debug_out( 'Zone_id('//clean_str(i_str)//'): '//clean_str(zone_id(i_zone)) )
        end do
    end do
    call progress_out
    
    ! check the uniqueness of zones id 
    call info_out( 'Check the uniqueness of zones id' )
    error_code = error_code+1
    call check_uniqueness( zone_id,size(zone_id),is_unique )
    if ( .not.is_unique ) call error_out( 'Zone ID must be unique.',exit_if_error )
    call debug_out( 'Each zones ID in all grid file is unique' )
    call progress_out
    
    ! parase zone name
    call info_out( 'get the names of each zone' )
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
    write (i_temp_str,*) n_zone
    call debug_out( 'Total n_zone is '//clean_str(i_temp_str) )
    
    ! clean up
    call core%destroy()

    end subroutine parse_grids
    
    !====================================================================
    

end module jc_grid_mod
