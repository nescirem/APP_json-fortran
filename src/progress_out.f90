!*****************************************************************************************
!> author: nescirem
!  date: 04/29/2019
!
!  Subroutine for show progressing.
!

subroutine progress_out
    
    use, intrinsic :: iso_fortran_env,  only: output_unit
    
    implicit none
    
    write( output_unit,"(A)",advance='no' ) '.'

    
end subroutine progress_out
