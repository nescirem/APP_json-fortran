!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
!  Module parse grid additional info.
!
    
module jc_grid_additional_mod

    use json_module, CK => json_CK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env,  only: error_unit, output_unit
    use common_data,    only: dir,filename,error_code,&
                            grid_file,grid_type
                                        
    implicit none

    private
    public :: jc_grid_additional

contains
    
    !-------------------------------------------------------------------+
    subroutine jc_grid_additional                                       !
    !-------------------------------------------------------------------+
    
    use common_data,    only: threeD,twoD,AXI2D,REVOLVE,&
                            scale,n_phase,is_homogeneous,have_interior_face,&
                            n_interface_pair,n_cycle_face_pair

    implicit none

    type(json_file)     :: json     ! the JSON structure read from the file
    
    integer(IK)                                 :: var_type
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    
    call json%initialize()
    
    ! parse the json file
    error_code = error_code+1
    call json%load_file( filename = dir//filename )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse JSON file' )
    end if
    call progress_out
    
    call json%get( 'additionalGridInfo.space', str_temp, found )
    error_code = error_code+1
    if ( .not.found ) call error_out( 'Must specify space type, please check: additionalGridInfo.space' )
    select case ( str_temp )
    case ( 'AXI2D' )
        AXI2D = .true.
    case ( '3D' )
        threeD = .true.
    case ( '2D' )
        twoD = .true.
    case ( 'REVOLVE' )
        REVOLVE = .true.
    case default
        call error_out( 'Unknown space type "'//str_temp//'", please check: additionalGridInfo.space' )
    end select
    call progress_out
    
    call json%get( 'additionalGridInfo.scale', scale, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: additionalGridInfo.scale' )
    end if
    if ( .not.found ) scale = 1.0d0 !default scale is 1.0
    call progress_out
    
    call json%get( 'additionalGridInfo.haveInteriorFace', have_interior_face, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: additionalGridInfo.haveInteriorFace' )
    end if
    if ( .not.found ) have_interior_face = .false. !default have no interior faces
    call progress_out
    
    call json%get( 'additionalGridInfo.numPairInterface', n_interface_pair, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: additionalGridInfo.numPairInterface' )
    end if
    if ( .not.found ) n_interface_pair = 0 !default no interfaces
    call progress_out
    
    call json%get( 'additionalGridInfo.numPairCyclePair', n_cycle_face_pair, found )
    error_code = error_code+1
    if ( json%failed() ) then
        call json%print_error_message( error_unit )
        call error_out( 'An error occurred during parse: additionalGridInfo.numPairCyclePair' )
    end if
    if ( .not.found ) n_cycle_face_pair = 0 ! default no cycle face pair
    call progress_out
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_grid_additional
    
    !====================================================================
    
    
end module jc_grid_additional_mod
