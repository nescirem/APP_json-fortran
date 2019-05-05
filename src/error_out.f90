!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Module print error message for debug.
!
module jc_error_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit

    implicit none

    private
    
    interface error_out
        module procedure jc_error_out
    end interface

    public :: error_out

contains
    
    subroutine jc_error_out ( err_message,terminate )
    
    use common_data,                    only: error_code
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: err_message
    logical,optional,intent(in)         :: terminate
    character(len=32)                   :: error_code_str
    
    integer,parameter                   :: set_len = 45
    logical,parameter                   :: default_terminate = .false.
    
    integer                             :: err_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    logical                             :: separate,l_terminate
    
    if (  present(terminate) ) then
        l_terminate = terminate
    else
        l_terminate = default_terminate
    end if
    
    err_message_len = len( clean_str(err_message) )
    nr_parts = real(err_message_len)/real(set_len)
    n_parts = ceiling( nr_parts )
    
    separate = .true.
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
    
    if ( l_terminate ) stop 'Program terminated.'
    
    end subroutine jc_error_out
    
    
end module jc_error_out_mod
