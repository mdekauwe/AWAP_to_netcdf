!--------------------------------------------------------------------------!
!		     		M A I N   P R O G R A M             	                         !
!--------------------------------------------------------------------------!
! USAGE:
! INCLUDE:
!--------------------------------------------------------------------------!
! include 'type_def.F90'
! include 'bios_io.F90'
! include 'cable_bios_met_obs_params.F90'
! include 'cable_weathergenerator.F90'
! include 'bios_output.F90'

PROGRAM awap_to_netcdf

    USE type_def_mod
    USE bios_io_mod              ! MMY
    USE cable_bios_met_obs_params
    USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
                               WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET
    USE bios_output


    IMPLICIT NONE

    REAL      :: dels  ! time step size in seconds
    INTEGER   :: kstart, kend, ktau, ktauday, counter, YYYY
    INTEGER   :: CurYear, YearStart, YearEnd ! MMY
    INTEGER   :: LOY

    REAL(sp),DIMENSION(:),ALLOCATABLE :: data_temp
    INTEGER(i4b)                      :: iunit


    ! input & output path
    CHARACTER(LEN = 200)   :: input, output


    ! input flt file path & name
    CHARACTER(LEN = 200)   :: rain_path, swdown_path, wind_path, &
                              tairmax_path, tairmin_path,        &
                              vph09_path, vph15_path

    ! output nc file path & name
    CHARACTER(LEN = 200)   :: Rainf_name, Snow_name, LWdown_name, &
                              SWdown_name, Tair_name, Wind_name,  &
                              Qair_name, PSurf_name
    ! output data netcdf file ID
    INTEGER                :: ncid_rain, ncid_snow, ncid_lw, ncid_sw,&
                              ncid_tair, ncid_wind, ncid_qair, ncid_ps

    ! output variable ID
    INTEGER                :: rainID, snowID, lwID, swID, &
                              tairID, windID, qairID, psID


    ! time variable ID for output nc file
    INTEGER                :: raintID, snowtID, lwtID, swtID, &
                              tairtID, windtID, qairtID, pstID

    TYPE(WEATHER_GENERATOR_TYPE) :: WG
    TYPE(FILE_NAME)              :: filename


! ************ 1. Initialise variable, arrays to store things, etc *************

    dels      = 10800.  ! It should be 3 hours = 3600*3. dels is time step size
                        ! in seconds given by bios.nml
    kstart    = 1
    ktauday   = INT(24.0*3600.0/dels) ! ktauday = 8

    YearStart = 2000
    YearEnd   = 2017
    CurYear   = YearStart


    CALL inout_path(filename)

    input = TRIM(filename%path_in)

    rain_path    = input//"/awap_rain_mm_day_2000-2017"
    swdown_path  = input//"/awap_rad_MJ_day_2000-2017"
    wind_path    = input//"/mcvicar_windspeed_ms_day_2000-2017"
    tairmax_path = input//"/awap_tmax_C_day_2000-2017"
    tairmin_path = input//"/awap_tmin_C_day_2000-2017"
    vph09_path   = input//"/awap_vph09_hpa_day_2000-2017"
    vph15_path   = input//"/awap_vph15_hpa_day_2000-2017"

    CALL cable_bios_init(dels, CurYear, kend, ktauday, rain_path, filename) ! removing met -- MMY
       ! INCLUDING:
       ! 1 CALL ReadArcFltHeader(iunit,landmaskhdr_file,MaskCols,MaskRows,MaskBndW,MaskBndS,MaskRes,NoDataVal)
       ! 2 CALL open_bios_met ! CANCELED BY MMY
       ! 3 CALL WGEN_INIT( WG, mland, latitude, dels )

    CALL read_filename(rain_path,filename%rain_file      )
    CALL read_filename(swdown_path,filename%swdown_file  )
    CALL read_filename(wind_path,filename%wind_file      )
    CALL read_filename(tairmax_path,filename%tairmax_file)
    CALL read_filename(tairmin_path,filename%tairmin_file)
    CALL read_filename(vph09_path,filename%vph09_file    )
    CALL read_filename(vph15_path,filename%vph15_file    )

    counter = 0
    PRINT *, "Initialization is ready"

	! 2. Loop over years
    DO YYYY = YearStart, YearEnd ! YYYY= CABLE_USER%YearStart,  CABLE_USER%YearEnd
       CurYear = YYYY
	     IF ((MOD(YYYY,4) == 0 .AND. MOD(YYYY,100) /= 0) .OR. MOD(YYYY,400) == 0)&
       THEN                     ! leap year
	        LOY = 366
	     ELSE
	        LOY = 365
       ENDIF

       PRINT *,"Point 6 CurYear, LOY ", CurYear, LOY ! Debug

       kend = NINT(24.0*3600.0/dels) * LOY ! rounds its argument to the nearest whole number.
                                           ! kend is the total timesteps of the current year
       output = TRIM(filename%path_out)

       Rainf_name   = output//"Rainf/AWAP.Rainf.3hr."//CHAR(CurYear)//".nc"
       Snow_name    = output//"Snowf/AWAP.Snowf.3hr."//CHAR(CurYear)//".nc"
       LWdown_name  = output//"LWdown/AWAP.LWdown.3hr."//CHAR(CurYear)//".nc"
       SWdown_name  = output//"SWdown/AWAP.SWdown.3hr."//CHAR(CurYear)//".nc"
       Tair_name    = output//"Tair/AWAP.Tair.3hr."//CHAR(CurYear)//".nc"
       Wind_name    = output//"Wind/AWAP.Wind.3hr."//CHAR(CurYear)//".nc"
       Qair_name    = output//"Qair/AWAP.Qair.3hr."//CHAR(CurYear)//".nc"
       PSurf_name   = output//"PSurf/AWAP.PSurf.3hr."//CHAR(CurYear)//".nc"

       CALL create_output_file(Rainf_name, ncid_rain, rainID, raintID, "Rainf",&
                              "Rainfall rate",                         &
                              "rainfall_flux",                         &
                              "Rainf", "kg m-2 s-1")

       CALL create_output_file(Snow_name, ncid_snow, snowID, snowtID, "Snowf", &
                              "Snowfall rate",                         &
                              "snowfall_flux",                         &
                              "Snowf","kg m-2 s-1")

       CALL create_output_file(LWdown_name, ncid_lw, lwID, lwtID, "LWdown",    &
                               "Downward Longwave Radiation",          &
                               "surface_downwelling_longwave_flux_in_air", &
                               "LWdown","W m-2")

       CALL create_output_file(SWdown_name, ncid_sw, swID, swtID, "SWdown",    &
                              "Downward Shortwave Radiation",          &
                              "surface_downwelling_shortwave_flux_in_air", &
                              "SWdown", "W m-2")

       CALL create_output_file(Tair_name, ncid_tair, tairID, tairtID,"Tair",   &
                              "Near surface air temperature",          &
                              "air_temperature",                       &
                              "Tair","K")

       CALL create_output_file(Wind_name, ncid_wind, windID, windtID, "Wind",  &
                              "Near surface wind speed",               &
                              "wind_speed",                            &
                              "Wind","m s-1")

       CALL create_output_file(Qair_name, ncid_qair, qairID, qairtID, "Qair",  &
                              "Near surface specific humidity",        &
                              "specific_humidity",                     &
                              "Qair", "kg kg-1")

       CALL create_output_file(PSurf_name, ncid_ps, psID, pstID, "PSurf",      &
                              "Surface Pressure",                      &
                              "surface_air_pressure",                  &
                              "PSurf", "Pa")


       DO ktau = kstart, kend

          CALL cable_bios_read_met( WG, filename, counter, CurYear, YearStart, YearEnd, ktau, kend, dels )
             ! INCLUDING:
             ! 1 CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(met%doy(1))+1 )
             ! 2 CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(met%hod(1)*3600./dels) )

          ALLOCATE(data_temp(mland))

          data_temp = WG%Precip
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .FALSE., ncid_rain, rainID, raintID)
          data_temp = WG%Snow
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .FALSE., ncid_snow, snowID, snowtID)
          data_temp = WG%PhiLd
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .TRUE., ncid_lw  , lwID  , lwtID  )
          data_temp = WG%PhiSd
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .TRUE., ncid_sw  , swID  , swtID  )
          data_temp = WG%Temp
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .FALSE., ncid_tair, tairID, tairtID)
          data_temp = WG%Wind
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .TRUE., ncid_wind, windID, windtID)
          data_temp = WG%QV
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .FALSE., ncid_qair, qairID, qairtID)
          data_temp = WG%PPa
          CALL write_output(filename, data_temp, dels, CurYear, ktau, kend, &
                            .FALSE., ncid_ps  , psID  , pstID  )
          DEALLOCATE(data_temp)

       END DO ! END Do loop over timestep ktau


       ok = NF90_CLOSE(ncid_rain)
       ok = NF90_CLOSE(ncid_snow)
       ok = NF90_CLOSE(ncid_lw  )
       ok = NF90_CLOSE(ncid_sw  )
       ok = NF90_CLOSE(ncid_tair)
       ok = NF90_CLOSE(ncid_wind)
       ok = NF90_CLOSE(ncid_qair)
       ok = NF90_CLOSE(ncid_ps  )

    END DO !YEAR

END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     		E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!
