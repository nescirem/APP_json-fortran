!*****************************************************************************************
!> author: nescirem
!  date: 5/05/2019
!
!  Module parse ASI output information.
!

module jc_asi_output_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    
    implicit none

    private
    public :: jc_asi_output
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_asi_output                                            !
    !-------------------------------------------------------------------+
    
    implicit none
    
    
    end subroutine jc_asi_output
    
    !====================================================================
    

end module jc_asi_output_mod
