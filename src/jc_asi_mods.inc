!GTEA acoustic solid interaction

!BASE MODULE
!==============================================================================
    use jc_grid_mod,                    only: jc_grid
    use jc_grid_control_mod,            only: jc_grid_control
    use jc_zone_mod,                    only: jc_zone
    use jc_solver_mod,                  only: jc_solver
    use jc_material_mod,                only: jc_material
    use jc_strategy_mod,                only: jc_strategy
    use jc_boundary_mod,                only: jc_boundary
    use jc_initial_mod,                 only: jc_initial
    use jc_output_mod,                  only: jc_output
	
!ASI ADDITIONAL MODULE
!==============================================================================
    use jc_asi_solver_mod,              only: jc_asi_solver
    use jc_asi_material_mod,            only: jc_asi_material
    use jc_asi_boundary_mod,            only: jc_asi_boundary
    use jc_asi_initial_mod,             only: jc_asi_initial
    use jc_asi_output_mod,              only: jc_asi_output
