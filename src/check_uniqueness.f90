!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module for check the uniqueness between array elements,
!  integer or character arrays are supported
!
    
module check_uniqueness_mod

    implicit none
  
    interface check_uniqueness
        module procedure check_uniqueness_ivec 
        module procedure check_uniqueness_cvec
    end interface
    
    private
    
    public :: check_uniqueness
 
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine check_uniqueness_ivec ( ivec,size_vec,is_unique )        !
    ! check the uniqueness between integer array elements               !
    !-------------------------------------------------------------------+
  
    implicit none
    integer, intent(in)     :: size_vec
    integer, intent(in)     :: ivec(size_vec)
    logical, intent(out)    :: is_unique
    
    integer                 :: b_int,c_int
    integer                 :: i,j
    
    is_unique = .true.
    
    baseloop: do i=1,size_vec-1
        b_int = ivec(i)
        do j=i+1,size_vec
            c_int = ivec(j)
            if ( c_int==b_int ) then
                is_unique = .false. 
                exit baseloop
            end if
        end do
    end do baseloop
    
    
    end subroutine check_uniqueness_ivec
    
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine check_uniqueness_cvec( cvec,size_vec,is_unique )         !
    ! check the uniqueness between character array elements             !
    ! " str  " = "str"   ->  .true.                                     !
    !-------------------------------------------------------------------+
    
    use functions,                  only: clean_str
  
    implicit none
    
    integer, intent(in)             :: size_vec
    character(len=*), intent(in)    :: cvec(size_vec)
    logical, intent(out)            :: is_unique
    
    character(len=:), allocatable   :: b_str,c_str
    integer                         :: i,j
    
    is_unique = .true.
    
    baseloop: do i=1,size_vec-1
        b_str = clean_str(cvec(i))
        do j=i+1,size_vec
            c_str = clean_str(cvec(j))
            if ( c_str==b_str ) then
                is_unique = .false. 
                exit baseloop
            end if
        end do
    end do baseloop
    
    
    end subroutine check_uniqueness_cvec
    
    !====================================================================
    

end module check_uniqueness_mod
