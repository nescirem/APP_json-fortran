!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Parameters and common data.
!
!  * Module of parameters
!  * Module of shared common data.
!
    
module parameters
    
    implicit none
    
    integer,parameter       :: dp = kind(1.0d0)
    integer,parameter       :: sp = kind(1.0e0)
    
end module parameters
    
    
module common_data
    
    use parameters
    
    implicit none
        
    integer                                     :: error_code = 0
    
    ! debug
    logical                                     :: exit_if_error
        
    ! working directory and control file name
    character(:),allocatable                    :: dir          ! working directory
    character(:),allocatable                    :: filename     ! control file to read
        
    ! grid file data
    integer                                     :: n_grid
    character(len=128),pointer,dimension(:)     :: grid_file
    integer,pointer,dimension(:)                :: grid_type
        
    ! zone data
    integer                                     :: n_zone
    character(len=32),pointer,dimension(:)      :: zone_id      ! max zone id never >= 1x10e33
    character(len=128),pointer,dimension(:)     :: zone_name
        
    ! grid infomation
    logical                                     :: threeD=.false.,twoD=.false.,&
                                                    AXI2D=.false.,REVOLVE=.false.
    real(dp)                                    :: scale
    logical                                     :: have_interior_face
    integer                                     :: n_interface_pair,n_cycle_face_pair
        
    ! zone information
    integer                                     :: n_phase
    character(len=128),pointer,dimension(:)     :: zone_material_id
    logical,pointer,dimension(:)                :: is_homogeneous
    integer,pointer,dimension(:)                :: zone_type
        
    ! solver settings
    integer                                     :: problem_type
    !! asi solver
    logical,pointer,dimension(:)                :: have_mean_flow,is_porous
    integer,pointer,dimension(:)                :: wave_flag,n_wave
    !! cfd solver
        
    ! material information
    integer                                     :: n_material
    type material
        character(len=:),allocatable            :: material_id, material_name
        real(dp)                                :: rho, heat_capacity_ratio
        !! ASI material
        real(dp)                                :: acoustic_velocity, flow_resistance
        real(dp)                                :: structure_constant, porousity, ambient_pressure
        !! NHT material
        logical                                 :: is_multiphase
        integer                                 :: ratio_flag  ! 1=volume persentage, 2=mass persentage 
        real(dp)                                :: ratio
    end type material
        
    type(material),pointer,dimension(:)         :: MTR
        
    ! discrete strategy settings
    integer                                     :: time_stategy ! 1=transient, 2=steaty
    real(dp)                                    :: time_start,time_end
    integer,pointer,dimension(:)                :: transient_formulation ! 1=explicit, 2=implicit
    real(dp),pointer,dimension(:)               :: delta_T
    integer,pointer,dimension(:)                :: gradient
        
        
end module common_data
