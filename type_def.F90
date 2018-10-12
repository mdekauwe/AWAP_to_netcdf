! ***************************** MMY *********************************** 
! ALL TYPE USED IN AWAP TO NETCDF
MODULE type_def_mod

   IMPLICIT NONE
   save
   
! From ./offline/cable_bios_met_obs_params.F90

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
  
! From cable_def_types_mod in cable_define_types.f90 
  INTEGER ::   mland  ! land grid cells 
  
! TYPE dmydate is from MODULE bios_type_def in cable_bios_met_obs_params.f90
  TYPE dmydate
     INTEGER(i4b):: Day
     INTEGER(i4b):: Month 
     INTEGER(i4b):: Year
  END TYPE dmydate
    
   TYPE out_varID_type ! output variable IDs in netcdf file
    INTEGER ::      SWdown, LWdown, Wind, Wind_E, PSurf,                       &
                    Tair, Qair, Rainf, Snowf, tvar
                    CO2air,                          &
                    Qle, Qh, Qg, NEE, SWnet,                                   &
                    LWnet, SoilMoist, SoilTemp, Albedo,                        &
                    visAlbedo, nirAlbedo, SoilMoistIce,                        &
                    Qs, Qsb, Evap, BaresoilT, SWE, SnowT,                      &
                    RadT, VegT, Ebal, Wbal, AutoResp,                          &
                    LeafResp, HeteroResp, GPP, NPP, LAI,                       &
                    ECanop, TVeg, ESoil, CanopInt, SnowDepth,                  &
                    HVeg, HSoil, Rnet, , CanT,Fwsoil
           
  END TYPE out_varID_type
   
MODULE type_def_mod