! ***************************** MMY ***********************************
! ALL TYPE USED IN AWAP TO NETCDF
MODULE type_def_mod
! ******************************************************************************
! USAGE:
! INCLUDE:
! ******************************************************************************

    IMPLICIT NONE
    save ????????? effective?????

! =============== From ./offline/cable_bios_met_obs_params.F90 =================

    ! Define integer kind parameters to accommodate the range of numbers usually
    ! associated with 4, 2, and 1 byte integers.
    integer,parameter :: i4b = selected_int_kind(9)
    integer,parameter :: i2b = selected_int_kind(4)
    integer,parameter :: i1b = selected_int_kind(2)
    ! Define single and double precision real kind parameters:
    ! * Kind(1.0)   defines sp as the machine's default size for single precision
    ! * Kind(1.0d0) defines dp as the machine's default size for double precision
    integer,parameter :: sp  = kind(1.0)
    integer,parameter :: dp  = kind(1.0d0)
    ! lgt is set to the default kind required for representing logical values.
    integer,parameter :: lgt = kind(.true.)

! =============== From cable_def_types_mod in cable_define_types.f90 ===========

    INTEGER ::   mland  ! land grid cells

    TYPE filename
         CHARACTER(LEN=200) :: path_in, path_out
         CHARACTER(LEN=200), DIMENSION(:), ALLOCATABLE :: rain_file,    &
                                                          swdown_file,  &
                                                          wind_file,    &
                                                          tairmax_file, &
                                                          tairmin_file, &
                                                          vph09_file,   &
                                                          vph15_file ! MMY
    END TYPE filename

END MODULE type_def_mod
