!*****************************************************************************************
!> author: nescirem
!  date: 5/05/2019
!
!  Module parse base boundary information.
!

module jc_boundary_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    
    implicit none

    private
    public :: jc_boundary
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_boundary                                              !
    !-------------------------------------------------------------------+
    
    implicit none
    
    
    end subroutine jc_boundary
    
    !====================================================================
    

end module jc_boundary_mod
