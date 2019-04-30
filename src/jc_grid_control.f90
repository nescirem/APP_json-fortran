!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module parse grid info.

module jc_grid_control_mod

    use json_module, CK => json_CK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env,  only: error_unit, output_unit
    use common_data,    only: dir,filename,error_code,&
                            grid_file,grid_type
                                        
    implicit none

    private
    public :: jc_grid_control

contains

    subroutine jc_grid_control
    
    use common_data,    only: n_grid

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    integer(IK)                 :: var_type
    
    call json%initialize()
    
    ! parse the json file:
    
    !write( error_unit,'(A)' ) '"'//dir//filename//'"'
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
    end if
    
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
    
    ! clean up
    call core%destroy()
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_grid_control
    
    
    subroutine parse_grid ( p_local )
    
    use common_data,    only: grid_file,grid_type,&
                            n_zone,zone_name,zone_type,zone_id
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p_local
    type(json_value),pointer            :: p_lower_1, p_temp
    type(json_core)                     :: core
    
    integer                     :: i            !! counter
    character(len=16)           :: i_str
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: zone_c_temp
    logical                                     :: found
    
    call core%initialize()
    
    error_code = error_code+1
    call core%get( p_local,'fileName', zone_c_temp, found )
    if ( found ) then
        write( grid_file(1),* ) zone_c_temp
        call progress_out
    else
        call error_out( 'Must specify grid file name, please check: grid.fileName' )
    end if
    
    error_code = error_code+1
    call core%get( p_local,'fileType', zone_c_temp, found )
    if ( found ) then
        call progress_out
        select case (zone_c_temp)
        case ( 'msh' )
            grid_type(1) = 1
        case ( 'cgns' )
            grid_type(1) = 2
        case default
            call error_out( 'Unknown grid file type "'//zone_c_temp//'", please check: grid.fileType' )
        end select
    else
        grid_type(1) = 2 ! default cgns
    endif
    
    ! get num of the elements
    call core%get( p_local,'zone', p_lower_1 )
    call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
    
    ! allocate zone data
    allocate( zone_id(n_zone), zone_name(n_zone),zone_type(n_zone) )
    
    ! parase the name/id of each elements
    do i=1,n_zone
        write( i_str,* ) i
        call core%get( p_lower_1,'@('//trim(i_str)//')',p_temp )
        call core%info( p_temp, name=zone_c_temp )
        write( zone_id(i),'(A)' ) zone_c_temp
    end do
    
    do i=1,n_zone
        
        call core%get( p_lower_1, trim(zone_id(i))//'.name', zone_c_temp, found )
        if ( found ) write( zone_name(i),* ) zone_c_temp
        
        call core%get( p_lower_1, trim(zone_id(i))//'.type', zone_c_temp, found )
        
        error_code = error_code+1
        if ( .not.found ) call error_out( 'Must specify zone type, please check: grid.zone.[zone_id].type' )
        
        error_code = error_code+1
        select case ( zone_c_temp )
        case ( 'fluid' )
            zone_type(i) = 1
        case ( 'solid' )
            zone_type(i) = 2
        case ( 'damping' )
            zone_type(i) = 3
        case ( 'acoustic' )
            zone_type(i) = 6
        case default
            call error_out( 'Unknown zone type "'//zone_c_temp//'", please check: grid.zone.[zone_id].type' )
        end select
        
    end do
    
    ! clean up
    call core%destroy()
    !call error_out( '' )
    
    end subroutine parse_grid
    
    subroutine parse_grids ( p_local )
    
    use common_data,    only: n_grid,grid_file,grid_type,&
                            n_zone,zone_name,zone_type,zone_id
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p_local
    type(json_value),pointer            :: p_lower_1, p_temp
    type(json_core)                     :: core
    
    integer                     :: i,i_grid            !! counter
    character(len=16)           :: i_str,i_zone_str
    integer                     :: i_zone = 0
    integer                     :: np_zone = 0
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: zone_c_temp
    logical                                     :: found
    
    call core%initialize()
    call progress_out
    
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        error_code = error_code+1
        call core%get( p_local,'@['//trim(i_str)//'].fileName', zone_c_temp, found )
        if ( found ) then
            write( grid_file(i_grid),* ) zone_c_temp
        else
            call error_out( 'Must specify grid file name, please check: grid.fileName' )
        end if
    
        error_code = error_code+1
        call core%get( p_local,'@['//trim(i_str)//'].fileType', zone_c_temp, found )
        if ( found ) then
            select case (zone_c_temp)
            case ( 'msh' )
                grid_type(i_grid) = 1
            case ( 'cgns' )
                grid_type(i_grid) = 2
            case default
                call error_out( 'Unknown grid file type "'//zone_c_temp//'", please check: grid.fileType' )
            end select
        else
            grid_type(i_grid) = 2 ! default cgns
        end if
    
        ! get num of the elements
        call core%get( p_local,'@['//trim(i_str)//'].zone', p_lower_1 )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        np_zone = np_zone+n_zone
    enddo
    ! allocate zone data
    allocate( zone_id(np_zone),zone_name(np_zone),zone_type(np_zone) )
    
    ! parase the name/id of each elements
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        call core%get( p_local,'@['//trim(i_str)//'].zone', p_lower_1 )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        do i=1,n_zone
            i_zone = i_zone+1
            write( i_zone_str,* ) i
            call core%get( p_lower_1,'@('//trim(i_zone_str)//')',p_temp )
            call core%info( p_temp, name=zone_c_temp )
            write( zone_id(i_zone),'(A)' ) zone_c_temp
        end do
    end do
    i_zone = 0
    do i_grid=1, n_grid
        write( i_str,* ) i_grid
        call core%get( p_local,'@['//trim(i_str)//'].zone', p_lower_1 )
        call core%info( p_lower_1, var_type=var_type, n_children=n_zone )
        do i=1,n_zone
            i_zone = i_zone+1
            call core%get( p_lower_1, trim(zone_id(i_zone))//'.name', zone_c_temp, found )
            if ( found ) write( zone_name(i_zone),* ) zone_c_temp
        
            call core%get( p_lower_1, trim(zone_id(i_zone))//'.type', zone_c_temp, found )
        
            call progress_out
            error_code = error_code+1
            if ( .not.found ) call error_out( 'Must specify zone type, please check: grid.zone.[zone_id].type' )
        
            call progress_out
            error_code = error_code+1
            select case ( zone_c_temp )
            case ( 'fluid' )
                zone_type(i_zone) = 1
            case ( 'solid' )
                zone_type(i_zone) = 2
            case ( 'damping' )
                zone_type(i_zone) = 3
            case ( 'acoustic' )
                zone_type(i_zone) = 6
            case default
                call error_out( 'Unknown zone type "'//zone_c_temp//'", please check: grid.zone.[zone_id].type' )
            end select
        
        end do
        
    end do
    
    n_zone = np_zone
    
    ! clean up
    call core%destroy()
    call progress_out

    end subroutine parse_grids
    

end module jc_grid_control_mod
