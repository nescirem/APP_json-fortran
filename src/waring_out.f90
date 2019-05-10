!*****************************************************************************************
!> author: nescirem
!  date: 05/06/2019
!
!  Module print warning message and write it to the log file.
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
    subroutine warning_out ( msg )                                      !
    !-------------------------------------------------------------------+
    
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: msg
    
    integer,parameter                   :: set_len = 45
    
    integer                             :: msg_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate
    
    if ( log_level>=2 ) then
        
        ! output to log file
        open(  NEWUNIT=unit, FILE=log_file, STATUS='OLD', POSITION='APPEND', IOSTAT=istat )
        write( unit,"(A)" ) '[WARNING] '//clean_str(msg)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
    
        ! scatter the warning message string if it is too long
        msg_len = len( clean_str(msg) )
        nr_parts = real(msg_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts==1 ) separate = .false.
    
        ! display warning message
        write( error_unit,"(A)" )               'WARNING'
        if ( separate ) then
            write( error_unit,"(A)" )           ' |-message: '//msg(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(A)")     '            '//msg(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(A)" )       '            '//msg(i*set_len+1:(i+1)*set_len)
            end do
        else !if ( .not.separate ) then
            write( error_unit,"(A)" )           ' |-message: '//clean_str(msg)
        end if
        
    end if
    
    end subroutine warning_out
    
    !====================================================================
    
    
end module jc_warning_out_mod
