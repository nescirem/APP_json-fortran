!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Module of parameters
!  Module of shared common data.
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
        logical                                     :: is_homogeneous
        integer,pointer,dimension(:)                :: zone_type
        
        ! solver settings
        integer                                     :: problem_type
        !! asi solver
        logical,pointer,dimension(:)                :: have_mean_flow,is_porous
        integer,pointer,dimension(:)                :: wave_flag,n_wave
        !! cfd solver
        
        
        
        
        
        
    end module common_data
