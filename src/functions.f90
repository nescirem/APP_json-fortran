!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
    
module functions
    
    implicit none
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    function clean_str(string)                                          !
    ! remove leading and trailing blank characters of a string          !
    !-------------------------------------------------------------------+
        
        implicit none
    
        character(:), allocatable   :: clean_str
        character(len=*)            :: string
        
        clean_str = trim(adjustl(string))
    
    end function clean_str
    
    !====================================================================
    
    
end module functions
