!*****************************************************************************************
!> author: nescirem
!  date: 5/05/2019
!
!  Module parse ASI boundary settings.
!

module jc_asi_boundary_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code
    use functions,                      only: clean_str
    use output_mod
    
    implicit none

    private
    public :: jc_asi_boundary
    
contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_asi_boundary                                          !
    !-------------------------------------------------------------------+
    
    !use common_data,            only: 
    
    implicit none
    
    
    end subroutine jc_asi_boundary
    
    !====================================================================
    

end module jc_asi_boundary_mod
