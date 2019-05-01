!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Subroutine error message output for debug.
!

subroutine error_out ( err_message )
    
    use common_data,                    only: error_code
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: err_message
    character(len=32)                   :: error_code_str
    
    integer,parameter                   :: set_len = 45
    integer                             :: err_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    logical                             :: separate = .true.
    
    err_message_len = len( clean_str(err_message) )
    nr_parts = real(err_message_len)/real(set_len)
    n_parts = ceiling( nr_parts )
    
    if ( n_parts == 1 ) separate = .false.

    write( error_code_str,* ) error_code
    write( error_unit,"(A)" )   'x'
    
    write( error_unit,"(A,/,A)" )           'ERROR',&
                                            ' |-code: '//clean_str(error_code_str)
    if ( separate ) then
        write( error_unit,"(A)" )           ' |-message: '//err_message(1:set_len)
        do i=1,n_parts-1
            if ( i==n_parts-1) then
                write(error_unit,"(A,/)")   '            '//err_message(i*set_len+1:)
                exit
            end if
            write( error_unit,"(A)" )       '            '//err_message(i*set_len+1:(i+1)*set_len)
        enddo
    else !if ( .not.separate ) then
        write( error_unit,"(A,/)" )           ' |-message: '//clean_str(err_message)
    end if
    
    stop 'Program terminated.'
    
end subroutine error_out
