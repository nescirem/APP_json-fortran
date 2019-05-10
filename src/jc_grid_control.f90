!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module parse grid control info.
!
    
module jc_grid_control_mod

    use json_module, CK => json_CK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            grid_file,grid_type
    use functions,                      only: clean_str
    use output_mod
                                        
    implicit none

    private
    public :: jc_grid_control

contains
    
    !-------------------------------------------------------------------+
    subroutine jc_grid_control                                          !
    !-------------------------------------------------------------------+
    
    use common_data,    only: threeD,twoD,AXI2D,REVOLVE,&
                            scale,n_phase,is_homogeneous,have_interior_face,&
                            n_interface_pair,n_cycle_face_pair

    implicit none

    type(json_file)     :: json     ! the JSON structure read from the file
    
    integer(IK)                                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    
    
    call info_out( 'Parse grid control info' )
    
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    call debug_out( 'Get:','gridControl.space' )
    call json%get( 'gridControl.space', str_temp, found )
    error_code = error_code+1
    if ( .not.found ) call error_out( 'Must specify space type, please check: gridControl.space',exit_if_error )
    select case ( str_temp )
    case ( 'AXI2D' )
        AXI2D = .true.
        call debug_out( 'The spatial information is "AXI2D"' )
    case ( '3D' )
        threeD = .true.
        call debug_out( 'The spatial information is "3D"' )
    case ( '2D' )
        twoD = .true.
        call debug_out( 'The spatial information is "2D"' )
    case ( 'REVOLVE' )
        REVOLVE = .true.
        call debug_out( 'The spatial information is "REVOLVE"' )
    case default
        call error_out( 'Unknown space type "'//str_temp//'", please check: gridControl.space',exit_if_error )
    end select
    
    call debug_out( 'Get:','gridControl.scale' )
    call json%get( 'gridControl.scale', scale, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: gridControl.scale',exit_if_error )
    end if
    if ( found ) then
        call debug_out( 'scale=',scale )
    else !if ( .not.found ) then
        scale = 1.0d0 !default scale is 1.0
        call warning_out( 'Haven''t define "gridControl.scale", the grid scale was set to default: 1.0d0' )
    end if
    
    call debug_out( 'Get:','gridControl.haveInteriorFace' )
    call json%get( 'gridControl.haveInteriorFace', have_interior_face, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: gridControl.haveInteriorFace',exit_if_error )
    end if
    if ( found ) then
        call debug_out( 'have_interior_face=',have_interior_face )
    else !if ( .not.found ) then
        have_interior_face = .false. !default have no interior faces
        call warning_out( 'Haven''t define "gridControl.haveInteriorFace", default no interior faces' )
    end if
    
    call debug_out( 'Get:','gridControl.numPairInterface' )
    call json%get( 'gridControl.numPairInterface', n_interface_pair, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: gridControl.numPairInterface',exit_if_error )
    end if
    if ( found ) then
        call debug_out( 'n_interface_pair= ',n_interface_pair )
    else !if ( .not.found ) then
        n_interface_pair = 0 !default no interfaces
        call warning_out( 'Haven''t define "gridControl.numPairInterface", default no interfaces' )
    end if
    
    call debug_out( 'Get:','gridControl.numPairInterface' )
    call json%get( 'gridControl.numPairCyclePair', n_cycle_face_pair, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: gridControl.numPairCyclePair',exit_if_error )
    end if
    if ( found ) then
        call debug_out( 'n_cycle_face_pair=',n_cycle_face_pair )
    else !if ( .not.found ) then
        n_cycle_face_pair = 0 ! default no cycle face pair
        call warning_out( 'Haven''t define "gridControl.numPairCyclePair", default no cycle face pair' )
    end if
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_grid_control
    
    !====================================================================
    
    
end module jc_grid_control_mod
