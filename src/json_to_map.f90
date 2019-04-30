!*****************************************************************************************
!> author: nescirem
!  date: 4/28/2019
!
!  Module which parse a MAP-like structure.

module jf_to_map_mod

    use json_module, CK => json_CK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env,  only: error_unit, output_unit
    use common_data,                    only: error_code,n_zone,zone_name,zone_type,zone_id

    implicit none

    private
    public :: jf_to_map
    
    character(len=*),parameter :: dir = '../files/inputs/'    !! working directory
    character(len=*),parameter :: filename = 'json_to_map.json'
    
contains

    subroutine jf_to_map

    !! Example of "fake MAP"

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_core)             :: core         !! factory for manipulating `json_value` pointers
    type(json_value),pointer    :: p, p_temp    !! a pointer for low-level manipulations
    
    integer                     :: i            !! counter
    character(len=16)           :: i_str
    integer(IK)                 :: var_type
    character(kind=CK,len=:),allocatable        :: zone_c_temp
    logical                                     :: found
    
    
    call json%initialize()
    
    ! parse the json file:
    
    write( error_unit,'(A)' ) '"'//dir//filename//'"'
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( '' )
    else ! print the parsed data to the console
        write( error_unit,'(A)' ) ''
        write( error_unit,'(A)') 'printing the file...'
        call json%print_file( int(error_unit,IK) )
        if ( json%failed() ) then
            call json%print_error_message( error_unit )
            call error_out( '' )
        end if
        call core%initialize()
    end if
    
    ! **************parse map*****************
    
    ! get num of the elements
    call json%get( 'grid', p )
    call core%info( p, var_type=var_type, n_children=n_zone )
    
    ! allocate zone data
    allocate( zone_id(n_zone), zone_name(n_zone),zone_type(n_zone) )
    
    ! parase the name/id of each elements
    do i=1,n_zone
        write( i_str,* ) i
        call core%get( p,'@('//trim(i_str)//')',p_temp )
        call core%info( p_temp, name=zone_c_temp )
        write( zone_id(i),'(A)' ) zone_c_temp
    end do
    
    do i=1,n_zone
        
        call json%get( 'grid.'//trim(zone_id(i))//'.name', zone_c_temp, found )
        if ( found ) write( zone_name(i),* ) zone_c_temp
        
        call json%get( 'grid.'//trim(zone_id(i))//'.type', zone_c_temp, found )
        
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
            call error_out( 'Unknown zone type '//zone_c_temp//', please check: grid.zone.[zone_id].type' )
        end select
        
    end do
    
    ! clean up
    call core%destroy()
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    !call error_out( '' )

    end subroutine jf_to_map

    end module jf_to_map_mod
!*****************************************************************************************

#ifdef MAP
!*****************************************************************************************
program json_to_map

    use jf_to_map_mod , only: jf_to_map
    
    implicit none

    call jf_to_map

end program json_to_map
!*****************************************************************************************
#endif
