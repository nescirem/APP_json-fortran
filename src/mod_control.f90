!*****************************************************************************************
!> author: nescirem
!  date: 04/30/2019
!
!  Module of control information
!
!@note Select the module according to the specific problem type
!


!-------------------------------------------------------------------+
module jc_base_mod                                                  !
!-------------------------------------------------------------------+
    
    use jc_grid_mod,                    only: jc_grid
    use jc_grid_control_mod,            only: jc_grid_control
    use jc_zone_mod,                    only: jc_zone
    !use jc_parallel_mod,                only: jc_parallel
    use jc_solver_mod,                  only: jc_solver
    use jc_material_mod,                only: jc_material
    use jc_strategy_mod,                only: jc_strategy
    use jc_boundary_mod,                only: jc_boundary
    use jc_initial_mod,                 only: jc_initial
    use jc_output_mod,                  only: jc_output
    
    implicit none
    
    public
    
end module jc_base_mod
    
!====================================================================
    
!-------------------------------------------------------------------+
module jc_ASI_mod          ! GTEA acoustic solid interaction        !
!-------------------------------------------------------------------+
    
    use jc_base_mod
    use jc_asi_solver_mod,                  only: jc_asi_solver
    use jc_asi_material_mod,                only: jc_asi_material
    use jc_asi_boundary_mod,                only: jc_asi_boundary
    use jc_asi_initial_mod,                 only: jc_asi_initial
    use jc_asi_output_mod,                  only: jc_asi_output
    
    implicit none
    
    public
    
end module jc_ASI_mod
    
!====================================================================
    
!-------------------------------------------------------------------+
module jc_NHT_mod         ! GTEA numerical heat transfer            !
!-------------------------------------------------------------------+
    
    use jc_base_mod
    use jc_nht_parallel_mod,                only: jc_nht_parallel
    use jc_nht_solver_mod,                  only: jc_nht_solver
    use jc_nht_material_mod,                only: jc_nht_material
    use jc_nht_boundary_mod,                only: jc_nht_boundary
    use jc_nht_initial_mod,                 only: jc_nht_initial
    use jc_nht_output_mod,                  only: jc_nht_output
    
    implicit none
    
    public
    
end module jc_NHT_mod
    
!====================================================================
