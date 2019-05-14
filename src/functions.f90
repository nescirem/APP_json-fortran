!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
    
module functions
    
    implicit none
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    pure function clean_str(string)                                     !
    ! remove leading and trailing blank characters of a string          !
    !-------------------------------------------------------------------+
        
        implicit none
    
        character(len=*),intent(in)     :: string
        character(:),allocatable        :: clean_str
        
        clean_str = trim(adjustl(string))
    
    end function clean_str
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    pure function quote_str(string)                                     !
    ! add double quotes at the beginning and end of a string            !
    !-------------------------------------------------------------------+
        
        implicit none
    
        character(len=*),intent(in)     :: string
        character(:),allocatable        :: quote_str
        
        quote_str = '"'//string//'"'
    
    end function quote_str
    
    !====================================================================
    
    
end module functions
