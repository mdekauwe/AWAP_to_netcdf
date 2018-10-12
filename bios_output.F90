MODULE bios_output

USE type_def_mod, ONLY: out_varID_type
USE netcdf ???? how to use this one

  IMPLICIT NONE
  PRIVATE
  PUBLIC open_output_file, write_output, close_output_file, create_restart
  INTEGER :: ncid_out ! output data netcdf file ID
  REAL :: missing_value = -999999.0 ! for netcdf output
  TYPE out_varID_type ! output variable IDs in netcdf file
  INTEGER ::      SWdown, LWdown, Wind, Wind_E, PSurf,                       &
                    Tair, Qair, Rainf, Snowf, CO2air,                          &
                    Qle, Qh, Qg, NEE, SWnet,                                   &
                    LWnet, SoilMoist, SoilTemp, Albedo,                        &
                    visAlbedo, nirAlbedo, SoilMoistIce,                        &
                    Qs, Qsb, Evap, BaresoilT, SWE, SnowT,                      &
                    RadT, VegT, Ebal, Wbal, AutoResp,                          &
                    LeafResp, HeteroResp, GPP, NPP, LAI,                       &
                    ECanop, TVeg, ESoil, CanopInt, SnowDepth,                  &
                    HVeg, HSoil, Rnet, tvar, CanT,Fwsoil, RnetSoil, SnowMelt, &
                    NBP, TotSoilCarb, TotLivBiomass, &
                    TotLittCarb, SoilCarbFast, SoilCarbSlow, SoilCarbPassive, &
                    LittCarbMetabolic, LittCarbStructural, LittCarbCWD, &
                    PlantCarbLeaf, PlantCarbFineRoot, PlantCarbWood, &
                    PlantTurnover, PlantTurnoverLeaf, PlantTurnoverFineRoot, &
                    PlantTurnoverWood, PlantTurnoverWoodDist, PlantTurnoverWoodCrowding, &
                    PlantTurnoverWoodResourceLim, dCdt, Area, LandUseFlux, patchfrac, &
                    vcmax, hc
  END TYPE out_varID_type
  TYPE(out_varID_type) :: ovid ! netcdf variable IDs for output variables
  TYPE(parID_type) :: opid ! netcdf variable IDs for output variables
  TYPE output_temporary_type
    REAL(KIND=4), POINTER, DIMENSION(:) :: SWdown ! 6 downward short-wave
                                                  ! radiation [W/m2]
    REAL(KIND=4), POINTER, DIMENSION(:) :: LWdown ! 7 downward long-wave
                                                  ! radiation [W/m2]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Rainf  ! 8 rainfall [kg/m2/s]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Snowf  ! 9 snowfall [kg/m2/s]
    REAL(KIND=4), POINTER, DIMENSION(:) :: PSurf  ! 10 surface pressure [Pa]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Tair   ! 11 surface air temperature
                                                  ! [K]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Qair   ! 12 specific humidity [kg/kg]
    REAL(KIND=4), POINTER, DIMENSION(:) :: CO2air ! 13 CO2 concentration [ppmv]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Wind   ! 14 windspeed [m/s]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Wind_N ! 15 surface wind speed, N
                                                  ! component [m/s]
    REAL(KIND=4), POINTER, DIMENSION(:) :: Wind_E ! 16 surface wind speed, E
                                                  ! component [m/s]

 END TYPE output_temporary_type
  TYPE(output_temporary_type), SAVE :: out
  INTEGER :: ok   ! netcdf error status
  
  
SUBROUTINE open_output_file(fileout,ncid_o)
    ! Creates netcdf output file, defines all variables
    ! and writes parameters to it if requested by user.
    REAL, INTENT(IN) :: dels ! time step size
    INTEGER :: xID, yID, zID, tID, landID, patchID ! dimension IDs
    INTEGER :: latID, lonID, llatvID, llonvID ! time,lat,lon variable ID
    INTEGER :: xvID, yvID   ! coordinate variable IDs for GrADS readability
    !    INTEGER :: surffracID         ! surface fraction varaible ID
    CHARACTER(LEN=200) :: fileout !todaydate, nowtime ! used to timestamp netcdf file
    integer, intent(out) :: ncid_o ! MMY
    INTEGER :: ncid_out ! output data netcdf file ID
    INTEGER :: xdimsize, ydimsize
    
    xdimsize = 841
    ydimsize = 681
    
    ! Create output file:
    ok = NF90_CREATE(TRIM(fileout), NF90_CLOBBER, ncid_out) ! file_name need to be defined
    ncid_o = ncid_out ! MMY
    ! Put the file in define mode:
    ok = NF90_REDEF(ncid_out)
    
    ! Define dimensions:
    ok = NF90_DEF_DIM(ncid_out, 'time', NF90_UNLIMITED, tID)
    ok = NF90_DEF_DIM(ncid_out, 'lat', xdimsize, xID) ???????????? where is xdimsize from
    ok = NF90_DEF_DIM(ncid_out, 'lon', ydimsize, yID) ???????????? where is ydimsize from
   
    ! Define "time" variable and its attributes:
    ok = NF90_DEF_VAR(ncid_out, 'time', NF90_DOUBLE, (/tID/), ovid%tvar)
    ok = NF90_PUT_ATT(ncid_out, ovid%tvar, 'long_name', "Time")
    ok = NF90_PUT_ATT(ncid_out, ovid%tvar, 'standard_name', "time")
    ok = NF90_PUT_ATT(ncid_out, ovid%tvar, 'calendar', "proleptic_gregorian")
    ok = NF90_PUT_ATT(ncid_out, ovid%tvar, 'units', "hours since 1871-01-01 00:00:00") 
    !!NB!!! "hours since 1871-01-01 00:00:00" need to be changed !!!!!!!!!!!
    !!!!!!! "proleptic_gregorian" is from GWSP 

    ! Define latitude and longitude variable (ALMA):
    ok = NF90_DEF_VAR(ncid_out, 'lat', NF90_FLOAT, (/xID, yID/), latID) !!!!!!!!! NB (/xID, yID/) need to match the (/lat,lon/) of vars
    ok = NF90_PUT_ATT(ncid_out, latID, 'long_name', "Latitude")
    ok = NF90_PUT_ATT(ncid_out, latID, 'standard_name', "latitude")
    ok = NF90_PUT_ATT(ncid_out, latID, 'axis', "Y")
    ok = NF90_PUT_ATT(ncid_out, latID, 'units', "degrees_north")
    
    
    ok = NF90_DEF_VAR(ncid_out, 'lon', NF90_FLOAT, (/xID, yID/), lonID)
    ok = NF90_PUT_ATT(ncid_out, lonID, 'long_name', "Longitude")
    ok = NF90_PUT_ATT(ncid_out, lonID, 'standard_name', "longitude")
    ok = NF90_PUT_ATT(ncid_out, lonID, 'axis', "X")
    ok = NF90_PUT_ATT(ncid_out, lonID, 'units', "degrees_east")
    
    ! Write "cordinate variables" to enable reading by GrADS:
    ok = NF90_DEF_VAR(ncid_out, 'lat', NF90_FLOAT, (/xID/), xvID)
    ok = NF90_PUT_ATT(ncid_out, xvID, 'units', 'degrees_north')
    ok = NF90_DEF_VAR(ncid_out, 'lon', NF90_FLOAT, (/yID/), yvID)
    ok = NF90_PUT_ATT(ncid_out, yvID, 'units', 'degrees_east')

    

END SUBROUTINE open_output_file


! From SUBROUTINE define_output_variable_r1 in cable_write.F90 
SUBROUTINE define_output_var(ncid, varID, vname, vunits, longname, standardname, almaname,&
                             xID, yID, tID)
!=============DEFINE OUTPUT VARIABLES=======================================
! Define met forcing variables in output file and allocate temp output vars:

    ! Subroutine for defining a real valued 1D variable
    INTEGER, INTENT(IN) :: ncid ! netcdf file ID
    INTEGER, INTENT(OUT) :: varID ! variable's netcdf ID
    ! netcdf dimension IDs
    INTEGER, INTENT(IN) :: xID, yID, tID
    !LOGICAL, INTENT(IN) :: writepatch ! write patch-specific info for this var?
    CHARACTER(LEN=*), INTENT(IN) :: vname ! name of variable
    CHARACTER(LEN=*), INTENT(IN) :: vunits ! variable units
    CHARACTER(LEN=*), INTENT(IN) :: longname ! full variable name
    CHARACTER(LEN=*), INTENT(IN) :: standardname ! standard variable name
    CHARACTER(LEN=*), INTENT(IN) :: almaname !alma variable name

    ! First, decide which grid to use. If user has forced grid using output%grid
    ! in the namelist file, use this grid. Else use format of met file.
    ! normal x-y-t mask grid
    ok = NF90_DEF_VAR(ncid, vname, NF90_FLOAT, (/xID, yID, tID/), varID)
    
    ! Define long name:
    ok = NF90_PUT_ATT(ncid,varID, 'long_name', longname)
    
    ! Define standard name:
    ok = NF90_PUT_ATT(ncid,varID, 'standard_name', standardname)
    
    ! Define alma name:
    ok = NF90_PUT_ATT(ncid,varID, 'alma_name', almaname)
    
    ! Define variable units:
    ok = NF90_PUT_ATT(ncid, varID, 'units', vunits)
    
    ! Define missing/fill values:
    ok = NF90_PUT_ATT(ncid, varID, '_fillvalue', 1.e+20)
       
       ????????????????????????
       ALLOCATE(out%SWdown(mp))  
       out%SWdown = 0.0 ! initialise 
       ????????????????????????
    ! End netcdf define mode:
    ok = NF90_ENDDEF(ncid_out)

    ! Write latitude and longitude variables:
    ok = NF90_PUT_VAR(ncid, latID, REAL(lat_all, 4))
    ok = NF90_PUT_VAR(ncid, lonID, REAL(lon_all, 4))

END SUBROUTINE define_output_var
                             
SUBROUTINE write_output(CurYear, ktau, met, ncid)
    ! Writes model output variables and, if requested, calls
    ! energy and mass balance routines. This subroutine is called
    ! each timestep, but may only write to the output file periodically,
    ! depending on whether the user has specified that output should be
    ! aggregated, e.g. to monthly or 6-hourly averages.
    REAL             :: delh ! time step size in hour
    INTEGER, INTENT(IN)           :: ktau ! timestep number in loop which include spinup
   
    REAL(r_2), DIMENSION(1) :: timetemp ! temporary variable for storing time
    INTEGER, SAVE :: YearStart
    
    delh = 3.    
    

    ! Write to temporary time variable:
    ! hours from 1900-01-01-00:00 
    if (CurYear .gt. 1900) then
       timetemp = DBLE((INT((CurYear - 1 - 1900)/4.)*(366+365*3) &
                  + MOD((curyear - 1 - 1900),4)*365)*24.&
                  + (ktau-1)*delh)
    else if (CurYear .eq. 1900) then
       timetemp = DBLE((ktau-1)*delh)
    else 
       print *, "error"
    end if 
    ! Write time variable for this output time step:
    ok = NF90_PUT_VAR(ncid_out, ovid%tvar, timetemp,start = (/ktau/), count = (/1/)) 
 
    ! SWdown:  downward short-wave radiation [W/m^2]
    ok = NF90_PUT_VAR(ncid_out, ovid%SWdown, REAL(out%SWdown, 4),start = (/1,1,ktau/),&
                       count = (/xdimsize, ydimsize, 1/)) ! write data to file
    ！！！！！ 替换变量 
    加属性
    ! CALL write_ovar(out_timestep, ncid_out, ovid%SWdown, 'SWdown',       &
    !                out%SWdown, ranges%SWdown, patchout%SWdown, 'default', met)
    ! SUBROUTINE write_output_variable_r1(ktau, ncid, varID, vname, var_r1,        &
    !                                  vrange, writepatch, dimswitch, met)

  END SUBROUTINE write_output
  

       
       
       
       
       
       
       
       
    
    dimensions:
	time = UNLIMITED ; // (2928 currently)
	lat = 360 ;
	lon = 720 ;
variables:
	double time(time) ;
		time:long_name = "Time" ;
		time:standard_name = "time" ;
		time:calendar = "proleptic_gregorian" ;
		time:units = "hours since 1871-01-01 00:00:00" ;
	float lat(lat) ;
		lat:long_name = "Latitude" ;
		lat:standard_name = "latitude" ;
		lat:axis = "Y" ;
		lat:units = "degrees_north" ;
	float lon(lon) ;
		lon:long_name = "Longitude" ;
		lon:standard_name = "longitude" ;
		lon:axis = "X" ;
		lon:units = "degrees_east" ;
	float SWdown(time, lat, lon) ;
		SWdown:long_name = "Downward Shortwave Radiation" ;
		SWdown:standard_name = "surface_downwelling_shortwave_flux_in_air" ;
		SWdown:alma_name = "SWdown" ;
		SWdown:amip_name = "rsds" ;
		SWdown:units = "W m-2" ;
		SWdown:_fillvalue = 1.e+20 ;
