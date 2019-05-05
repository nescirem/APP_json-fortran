!*****************************************************************************************
!> author: nescirem
!  date: 5/05/2019
!
!  Module parse base output information.
!

module jc_output_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use jc_error_out_mod
    use jc_progress_out_mod
    
    implicit none

    private
    public :: jc_output
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_output                                                !
    !-------------------------------------------------------------------+
    
    implicit none
    
    
    end subroutine jc_output
    
    !====================================================================
    

end module jc_output_mod
