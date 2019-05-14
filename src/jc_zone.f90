!*****************************************************************************************
!> author: nescirem
!  date: 4/30/2019
!
!  Module parse zones information.
!

module jc_zone_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env,  only: error_unit
    use common_data,                    only: exit_if_error,dir,filename,error_code,&
                                            n_zone,zone_id
    use functions,                      only: clean_str
    use output_mod
    
    implicit none

    private
    public :: jc_zone

contains
    !====================================================================
    
    !-------------------------------------------------------------------+
    subroutine jc_zone                                                  !
    !-------------------------------------------------------------------+
    
    use common_data,    only: zone_material_id,is_homogeneous

    implicit none

    type(json_file)             :: json         !! the JSON structure read from the file
    
    character(kind=CK,len=:),allocatable        :: str_temp
    logical                                     :: found
    integer                                     :: i
    
    
    call info_out( 'Parse zones information' )
    
    call json%initialize()
    
    allocate ( zone_material_id(n_zone),is_homogeneous(n_zone) )
    
    ! parse the json file
    call debug_out( 'Load form file: "'//dir//filename//'"' )
    call json%load_file( filename = dir//filename )
    
    error_code = error_code+1
    do i=1,n_zone
        
        call debug_out( 'Get:','zone.'//clean_str(zone_id(i))//'.material' )
        call json%get( 'zone.'//clean_str(zone_id(i))//'.material', str_temp, found )
        if ( found ) then
            write( zone_material_id(i),* ) str_temp
            call debug_out( 'The material ID in zone["'//clean_str(zone_id(i))&
                //'"] is',zone_material_id(i) )
        else !if ( .not.found ) then
            call error_out( 'Must specify zone material, please check: zone.'&
                    //clean_str(zone_id(i))//'.material',exit_if_error )
        end if
        
        call debug_out( 'Get:','zone.'//clean_str(zone_id(i))//'.isHomogeneous' )
        call json%get( 'zone.'//clean_str(zone_id(i))//'.isHomogeneous',is_homogeneous(i), found )
        if ( found ) then
            call debug_out( 'isHomogeneous=',is_homogeneous(i) )
        else !if ( .not.found ) then
            is_homogeneous(i) = .true. ! default homogeneous
            call warning_out( 'Haven''t define "zone.'//clean_str(zone_id(i))//'.isHomogeneous",'&
                    //' default homogeneous' )
        end if
        
    end do
    
    ! clean up
    call json%destroy()
    
    end subroutine jc_zone
    
    !====================================================================
    
    
end module jc_zone_mod
