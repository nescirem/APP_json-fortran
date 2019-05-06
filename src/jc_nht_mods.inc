!GTEA numerical heat transfer modules

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

!NHT ADDITIONAL MODULE
!==============================================================================
    use jc_nht_parallel_mod,                only: jc_nht_parallel
    use jc_nht_solver_mod,                  only: jc_nht_solver
    use jc_nht_material_mod,                only: jc_nht_material
    use jc_nht_boundary_mod,                only: jc_nht_boundary
    use jc_nht_initial_mod,                 only: jc_nht_initial
    use jc_nht_output_mod,                  only: jc_nht_output
