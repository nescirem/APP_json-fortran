!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse base solver settings.
!

module jc_solver_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code
    use functions,                      only: clean_str
    use output_mod
    
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
    
    
    call info_out( 'Parse base solver settings' )
    call json%initialize()
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    error_code = error_code+1
    call debug_out( 'Get:','solver.problemType' )
    call json%get( 'solver.problemType', str_temp, found )
    if ( .not.found ) call error_out( 'Must specify problem solver, please check: solver.problemType',exit_if_error )
    select case ( str_temp )
    case ( 'acoustic solid interaction' )
        problem_type = 1
        call debug_out( 'The problem_type is "ASI"' )
    case ( 'ASI' )
        problem_type = 1
        call debug_out( 'The problem_type is "ASI"' )
    case ( 'CAA' )
        problem_type = 2
        call debug_out( 'The problem_type is "CAA"' )
    case ( 'NHT' )
        problem_type = 3
        call debug_out( 'The problem_type is "NHT"' )
    case ( 'chemical kinetics' )
        problem_type = 4
        call debug_out( 'The problem_type is "chemical kinetics"' )
    case ( 'structural mechanics' )
        problem_type = 5
        call debug_out( 'The problem_type is "structural mechanics"' )
    case default
        call error_out( 'Unknown problem solver "'//str_temp//'", please check: solver.problemType',exit_if_error )
    end select
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_solver
    
    !====================================================================
    
    
end module jc_solver_mod
