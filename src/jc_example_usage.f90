!*****************************************************************************************
!> author: nescirem
!  date: 4/28/2019
!
!  Entry point for the assemblaged example of json control file.
!
    
#ifdef ASSEMBLAGE
program jc_example_usage
    
    use, intrinsic :: iso_fortran_env,  only: output_unit
    use jc_CAA_mod
    use common_data,                    only: error_code,dir,filename
    use functions,                      only: clean_str
    
    implicit none
    
    dir = clean_str( '  ../files/inputs/' )
    filename = clean_str(  ' case_control.json ' )
    
    ! input grid control
    call jc_grid_control
    
    ! parse grid file
    !call read_cgns
    
    ! input additional grid info
    call jc_grid_additional
    
    ! input zone settings
    call jc_zone
    
    write( output_unit,"(A)" ) 'Done.'
    
end program jc_example_usage
#endif
