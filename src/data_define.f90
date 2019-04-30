!*****************************************************************************************
!> author: nescirem
!  date: 04/28/2019
!
!  Module of shared common data.
!

    module common_data
    
        implicit none
        
        integer                                     :: error_code = 0
        
        ! working directory and control file name
        character(:), allocatable                   :: dir           ! working directory
        character(:), allocatable                   :: filename      ! control file to read
        
        ! grid file data
        integer                                     :: n_grid
        character(len=128),pointer,dimension(:)     :: grid_file
        integer,pointer,dimension(:)                :: grid_type
        
        ! zone data
        integer                                     :: n_zone
        character(len=32),pointer,dimension(:)      :: zone_id !! max zone id never >= 1x10e33
        character(len=128),pointer,dimension(:)     :: zone_name
        integer,pointer,dimension(:)                :: zone_type
        
        ! grid infomation
        logical                                     :: threeD,twoD
        
    end module common_data
