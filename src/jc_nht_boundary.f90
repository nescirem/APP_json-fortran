!*****************************************************************************************
!> author: nescirem
!  date: 5/05/2019
!
!  Module parse NHT time-dependent boundary information.
!

module jc_nht_boundary_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    
    implicit none

    private
    public :: jc_nht_boundary
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_nht_boundary                                          !
    !-------------------------------------------------------------------+
    
    implicit none
    
    
    end subroutine jc_nht_boundary
    
    !====================================================================
    

end module jc_nht_boundary_mod
