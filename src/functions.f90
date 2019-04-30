!*****************************************************************************************
!> author: nescirem
!  date: 4/29/2019
!
    
module functions
    
    implicit none
    
    contains
    
    function clean_str(string)
    
        character(:), allocatable   :: clean_str
        character(len=*)            :: string
        
        clean_str = trim(adjustl(string))
    
    end function clean_str
    
end module functions
