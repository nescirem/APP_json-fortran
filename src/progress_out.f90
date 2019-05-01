!*****************************************************************************************
!> author: nescirem
!  date: 04/29/2019
!
!  Subroutine showing the progress.
!

subroutine progress_out
    
    use, intrinsic :: iso_fortran_env,  only: output_unit
    
    implicit none
    
    integer,parameter   :: line_len = 57
    integer,save        :: output_count = 0
    
    
    output_count = output_count+1
    
    if ( output_count==line_len ) then
        write( output_unit,"(A)" ) '.'
        output_count = 0
    else
        write( output_unit,"(A)",advance='no' ) '.'
    end if
    
    
end subroutine progress_out
