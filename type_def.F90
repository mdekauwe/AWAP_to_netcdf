! ***************************** MMY ***********************************
! ALL TYPE USED IN AWAP TO NETCDF
MODULE type_def_mod
! ******************************************************************************
! USAGE:
! INCLUDE:
! ******************************************************************************

    IMPLICIT NONE
    save !????????? effective?????

! =============== From ./offline/cable_bios_met_obs_params.F90 =================

    ! Define integer kind parameters to accommodate the range of numbers usually
    ! associated with 4, 2, and 1 byte integers.
    INTEGER, PARAMETER :: i4b = selected_int_kind(9)
    INTEGER, PARAMETER :: i2b = selected_int_kind(4)
    INTEGER, PARAMETER :: i1b = selected_int_kind(2)
    INTEGER, PARAMETER :: r_2 = SELECTED_REAL_KIND(12, 50) ! From cable_define_types.F90
    ! Define single and double precision real kind parameters:
    ! * Kind(1.0)   defines sp as the machine's default size for single precision
    ! * Kind(1.0d0) defines dp as the machine's default size for double precision
    INTEGER, PARAMETER :: sp  = kind(1.0)
    INTEGER, PARAMETER :: dp  = kind(1.0d0)
    ! lgt is set to the default kind required for representing logical values.
    INTEGER, PARAMETER :: lgt = kind(.true.)

! =============== From cable_def_types_mod in cable_define_types.f90 ===========

    INTEGER ::   mland  ! land grid cells

    TYPE FILE_NAME
         CHARACTER(LEN=200) :: path_in, path_out
         CHARACTER(LEN=200), DIMENSION(:), ALLOCATABLE :: rain_file,    &
                                                          swdown_file,  &
                                                          wind_file,    &
                                                          tairmax_file, &
                                                          tairmin_file, &
                                                          vph09_file,   &
                                                          vph15_file ! MMY
    END TYPE FILE_NAME
   
    !TYPE(FILE_NAME)      :: filename
END MODULE type_def_mod
