!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse base solver settings.
!

module jc_solver_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    use common_data,                    only: exit_if_error,dir,filename,error_code
    use functions,                      only: clean_str
    
    implicit none

    private
    public :: jc_solver

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_solver                                                !
    !-------------------------------------------------------------------+
    
    use common_data,    only: problem_type,zone_type

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    type(json_value),pointer    :: p            !! a pointer for low-level manipulations
    
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    
    
    call json%initialize()
    
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
    call json%get( 'solver.problemType', str_temp, found )
    if ( .not.found ) call error_out( 'Must specify problem solver, please check: solver.problemType',exit_if_error )
    select case ( str_temp )
    case ( 'acoustic solid interaction' )
        problem_type = 1
    case ( 'ASI' )
        problem_type = 1
    case ( 'CAA' )
        problem_type = 2
    case ( 'CFD' )
        problem_type = 3
    case ( 'chemical kinetics' )
        problem_type = 4
    case ( 'structural mechanics' )
        problem_type = 5
    case default
        call error_out( 'Unknown problem solver "'//str_temp//'", please check: solver.problemType',exit_if_error )
    end select
    call progress_out
    
    ! clean up
    call json%destroy()
    if ( json%failed() ) call json%print_error_message( error_unit )
    
    end subroutine jc_solver
    
    !====================================================================
    
    
end module jc_solver_mod
