!*****************************************************************************************
!> author: nescirem
!  date: 4/28/2019
!
!  Entry point for the assemblaged example of json control file.
!
!### See also
!  * [APP_json-fortran development site](https://github.com/nescirem/APP_json-fortran)
!  * [json-fortran development site](http://github.com/jacobwilliams/json-fortran)
!  * [json-fortran online documentation](http://jacobwilliams.github.io/json-fortran)
!  * [JSON website](http://www.json.org/)
!  * [JSON validator](http://jsonlint.com/)
!
!@note Originally JSON-Fortran was entirely contained within this module.
    
program jc_example_usage
    
    use jc_api_mod,                     only: jc_api
#include "jc_asi_mods.inc"
    use common_data,                    only: exit_if_error,dir,filename
    
    implicit none
    
    exit_if_error = .true.
    dir = '../files/inputs/'
    filename = 'case_control.json'
    
    ! API prepare
    call jc_api
    
    ! input grid information
    call jc_grid
    
    ! parse grid file
    !call read_cgns
    
    ! input grid control info
    call jc_grid_control
    
    ! input zone settings
    call jc_zone
    
    ! input solver settings
    call jc_solver
    !! input asi solver settings
    call jc_asi_solver
    
    ! input material info
    call jc_material
    !! input asi material info
    call jc_asi_material
    
    ! input discrete strategy settings
    call jc_strategy
    
    
end program jc_example_usage
