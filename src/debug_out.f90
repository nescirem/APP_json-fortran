!*****************************************************************************************
!> author: nescirem
!  date: 05/06/2019
!
!  Module print debug message for debug.
!
    
module jc_debug_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: log_file,log_level

    implicit none

    private

    public :: debug_out

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine debug_out ( debug_message )                             !
    !-------------------------------------------------------------------+
    
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: debug_message
    
    integer,parameter                   :: set_len = 57
    
    integer                             :: debug_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate
    
    if ( log_level>=4 ) then
        
        ! output to log file
        open (  NEWUNIT=unit,FILE=log_file,STATUS='OLD',POSITION='APPEND',IOSTAT=istat )
        write (unit,"(A)") '[DEBUG] '//clean_str(debug_message)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
    
        ! scatter the debug message string if it is too long
        debug_message_len = len( clean_str(debug_message) )
        nr_parts = real(debug_message_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts == 1 ) separate = .false.
    
        ! debug output
        write( error_unit,"(A)" )   ''
    
        ! display debug message
        if ( separate ) then
            write( error_unit,"(A)" )           debug_message(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(A,/)")   debug_message(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(A)" )       debug_message(i*set_len+1:(i+1)*set_len)
            enddo
        else !if ( .not.separate ) then
            write( error_unit,"(A,/)" )         clean_str(debug_message)
        end if
        
    end if
    
    end subroutine debug_out
    
    !====================================================================
    
    
end module jc_debug_out_mod