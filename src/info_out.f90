!*****************************************************************************************
!> author: nescirem
!  date: 05/06/2019
!
!  Module print info message and write it to the log file.
!
    
module jc_info_out_mod

    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: log_file,log_level
    
    implicit none

    private

    public :: info_out

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine info_out ( info_message )                                !
    !-------------------------------------------------------------------+
    
    use functions,                      only: clean_str
    
    implicit none

    character(len=*),intent(in)         :: info_message
    
    integer,parameter                   :: set_len = 45
    
    integer                             :: info_message_len
    real                                :: nr_parts
    integer                             :: n_parts,i
    integer                             :: unit,istat
    logical                             :: separate
    
    if ( log_level>=3 ) then
        
        ! output to log file
        open (  NEWUNIT=unit, FILE=log_file, STATUS='OLD', POSITION='APPEND', IOSTAT=istat )
        write( unit,"(G0)" ) '[INFO] '//clean_str(info_message)
        if ( istat==0 ) close( UNIT=unit, IOSTAT=istat )
    
        ! scatter the info message string if it is too long
        info_message_len = len( clean_str(info_message) )
        nr_parts = real(info_message_len)/real(set_len)
        n_parts = ceiling( nr_parts )
    
        separate = .true.
        if ( n_parts==1 ) separate = .false.
    
        ! display info message
        write( error_unit,"(G0)" )               'INFO'
        if ( separate ) then
            write( error_unit,"(G0)" )           ' |-message: '//info_message(1:set_len)
            do i=1,n_parts-1
                if ( i==n_parts-1) then
                    write(error_unit,"(G0)")     '            '//info_message(i*set_len+1:)
                    exit
                end if
                write( error_unit,"(G0)" )       '            '//info_message(i*set_len+1:(i+1)*set_len)
            end do
        else !if ( .not.separate ) then
            write( error_unit,"(G0)" )           ' |-message: '//clean_str(info_message)
        end if
        
    end if
    
    end subroutine info_out
    
    !====================================================================
    
    
end module jc_info_out_mod