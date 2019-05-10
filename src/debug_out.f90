!*****************************************************************************************
!> author: nescirem
!  date: 05/06/2019
!
!  Module that print debug message and write it to the log file.
!
!  call debug_out( msg, {int/arg/rel/str}, suf_str )
!
!@note Example Useage:
!@note ...
!@note use jc_debug_out_mod
!@note ...
!@note call debug_out( character_string )
!@note call debug_out( character_string, integer )
!@note call debug_out( character_string, integer, suffix_character_string )
!@note call debug_out( character_string, logical )
!@note call debug_out( character_string, logical, suffix_character_string )
!@note call debug_out( character_string, real )
!@note call debug_out( character_string, real, suffix_character_string )
!@note call debug_out( character_string, character_string )
!@note call debug_out( character_string, character_string, suffix_character_string )
!@note ...
!
    
module jc_debug_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: log_file,log_level
    use functions,                      only: clean_str

    implicit none
    
    interface debug_out
        module procedure debug_out_basic
        module procedure debug_out_int
        module procedure debug_out_arg
        module procedure debug_out_rel
        module procedure debug_out_str
    end interface
    
    private
    
    public :: debug_out

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out_basic ( msg )                                  !
    !-------------------------------------------------------------------+
    
    implicit none

    character(len=*),intent(in)         :: msg
    
    integer,parameter                   :: set_len = 57
    
    integer                             :: msg_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate
    
    if ( log_level>=4 ) then
        
        ! output to log file
        open (  NEWUNIT=unit,FILE=log_file,STATUS='OLD',POSITION='APPEND',IOSTAT=istat )
        write (unit,"(A)") '[DEBUG] '//clean_str(msg)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
    
        ! scatter the debug message string if it is too long
        msg_len = len( clean_str(msg) )
        nr_parts = real(msg_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts == 1 ) separate = .false.
    
        ! debug output
        write( error_unit,"(A)" )   ''
    
        ! display debug message
        if ( separate ) then
            write( error_unit,"(A)" )           msg(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(A)")     msg(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(A)" )       msg(i*set_len+1:(i+1)*set_len)
            end do
        else !if ( .not.separate ) then
            write( error_unit,"(A)" )           clean_str(msg)
        end if
        
    end if
    
    end subroutine debug_out_basic
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out_int ( msg, int, suf_str )                      !
    !-------------------------------------------------------------------+
    
    implicit none
    
    character(len=*),intent(in)         :: msg
    integer,intent(in)                  :: int
    character(len=*),optional,intent(in):: suf_str
    
    character(len=32)                   :: int_str
    character(len=:),allocatable        :: combined_msg
    
    
    write( int_str,* ) int
    if (  present(suf_str) ) then
        combined_msg = msg//' '//clean_str(int_str)//' '//suf_str
    else
        combined_msg = msg//' '//clean_str(int_str)
    end if
    call debug_out_basic( combined_msg )
    
    end subroutine debug_out_int
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out_arg ( msg, arg, suf_str )                      !
    !-------------------------------------------------------------------+
    
    implicit none
    
    character(len=*),intent(in)         :: msg
    logical,intent(in)                  :: arg
    character(len=*),optional,intent(in):: suf_str
    
    character(len=32)                   :: arg_str
    character(len=:),allocatable        :: combined_msg
    
    
    write( arg_str,"(L1)" ) arg
    if (  present(suf_str) ) then
        combined_msg = msg//' '//clean_str(arg_str)//' '//suf_str
    else
        combined_msg = msg//' '//clean_str(arg_str)
    end if
    call debug_out_basic( combined_msg )
    
    end subroutine debug_out_arg
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out_rel ( msg, rel, suf_str )                      !
    !-------------------------------------------------------------------+
    
    implicit none
    
    character(len=*),intent(in)         :: msg
    real(kind=8),intent(in)             :: rel
    character(len=*),optional,intent(in):: suf_str
    
    character(len=16)                   :: rel_str
    character(len=:),allocatable        :: combined_msg
    
    
    write( rel_str,"(E16.7E3)" ) rel
    if (  present(suf_str) ) then
        combined_msg = msg//' '//rel_str//' '//suf_str
    else
        combined_msg = msg//' '//rel_str
    end if
    call debug_out_basic( combined_msg )
    
    end subroutine debug_out_rel
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out_str ( msg, str, suf_str )                      !
    !-------------------------------------------------------------------+
    
    use functions,                      only: quote_str,clean_str
    
    implicit none
    
    character(len=*),intent(in)         :: msg
    character(len=*),intent(in)         :: str
    character(len=*),optional,intent(in):: suf_str
    
    character(len=:),allocatable        :: combined_msg
    
    if (  present(suf_str) ) then
        combined_msg = msg//' '//quote_str(clean_str(str))//' '//suf_str
    else
        combined_msg = msg//' '//quote_str(clean_str(str))
    end if
    call debug_out_basic( combined_msg )
    
    end subroutine debug_out_str
    
    !====================================================================
    
    
end module jc_debug_out_mod
