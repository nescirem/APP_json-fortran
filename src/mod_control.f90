!*****************************************************************************************
!> author: nescirem
!  date: 04/30/2019
!
!  Module of control information
!
    
    
!====================================================================
    
!-------------------------------------------------------------------+
module jc_base_mod                                                  !
!-------------------------------------------------------------------+
    
    use jc_grid_control_mod,            only: jc_grid_control
    use jc_grid_additional_mod,         only: jc_grid_additional
    use jc_zone_mod,                    only: jc_zone
    !use jc_parallel_mod,                only: jc_parallel
    use jc_solver_mod,                  only: jc_solver
    !use jc_material_mod,                only: jc_material
    
    implicit none
    
end module jc_base_mod
    
    
!====================================================================
    
!-------------------------------------------------------------------+
module jc_ASI_mod                                                   !
!-------------------------------------------------------------------+
    
use jc_base_mod
use jc_asi_solver_mod,                  only: jc_asi_solver
!use jc_asi_material_mod,                only: jc_asi_material
    
implicit none
    
end module jc_ASI_mod
    
    
!====================================================================
    
!-------------------------------------------------------------------+
module jc_CFD_mod                                                   !
!-------------------------------------------------------------------+
    
use jc_base_mod
!use jc_cfd_solver_mod,                  only: jc_cfd_solver
!use jc_cfd_material_mod,                only: jc_cfd_material
    
implicit none
    
end module jc_CFD_mod
    
!====================================================================
