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
    
    !-------------------------------------------------------------------+
    function quote_str(string)                                          !
    ! add double quotes at the beginning and end of a string            !
    !-------------------------------------------------------------------+
        
        implicit none
    
        character(:), allocatable   :: quote_str
        character(len=*)            :: string
        
        quote_str = '"'//string//'"'
    
    end function quote_str
    
    !====================================================================
    
    
end module functions
