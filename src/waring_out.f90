!*****************************************************************************************
!> author: nescirem
!  date: 05/06/2019
!
!  Module print warning message for debug.
!
    
module jc_warning_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: log_file,log_level

    implicit none

    private

    public :: warning_out

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine warning_out ( warning_message )                            !
    !-------------------------------------------------------------------+
    
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: warning_message
    
    integer,parameter                   :: set_len = 45
    
    integer                             :: warning_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate
    
    if ( log_level>=2 ) then
        
        ! output to log file
        open (  NEWUNIT=unit,FILE=log_file,STATUS='OLD',POSITION='APPEND',IOSTAT=istat )
        write (unit,"(A)") '[WARNING] '//clean_str(warning_message)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
    
        ! scatter the warning message string if it is too long
        warning_message_len = len( clean_str(warning_message) )
        nr_parts = real(warning_message_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts == 1 ) separate = .false.
    
        ! warning output
        write( error_unit,"(A)" )   ''
    
        ! display warning message
        write( error_unit,"(A)" )               'WARNING'
        if ( separate ) then
            write( error_unit,"(A)" )           ' |-message: '//warning_message(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(A,/)")   '            '//warning_message(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(A)" )       '            '//warning_message(i*set_len+1:(i+1)*set_len)
            enddo
        else !if ( .not.separate ) then
            write( error_unit,"(A,/)" )         ' |-message: '//clean_str(warning_message)
        end if
        
    end if
    
    end subroutine warning_out
    
    !====================================================================
    
    
end module jc_warning_out_mod
