!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Subroutine error message output for debug.
!

subroutine error_out ( err_message )
    
    use common_data,                    only: error_code
    use, intrinsic :: iso_fortran_env,  only: error_unit
    
    implicit none
    
    character(len=*),intent(in)         :: err_message
    character(len=32)                   :: error_code_str
    
    
    write( error_code_str,* ) error_code
    write( error_unit,"(A)" ) ''
    write( error_unit,"(3(A,/))" ) 'ERROR',&
                                   ' |-code: '//trim(adjustl(error_code_str)),&
                                   ' |-message: '//trim(err_message)
    
    stop 'Program terminated.'
    
end subroutine error_out
