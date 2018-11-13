!--------------------------------------------------------------------------!
!             	     M A I N   P R O G R A M                               !
!--------------------------------------------------------------------------!
! USAGE: This is the main program for AWAP_TO_NETCDF
! INCLUDE:type_def_mod, bios_io_mod, cable_bios_met_obs_params, &
!         cable_weathergenerator, bios_output
!--------------------------------------------------------------------------!

PROGRAM awap_to_netcdf

    USE type_def_mod
    USE bios_io_mod              ! MMY
    USE cable_bios_met_obs_params
    USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
                               WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET
    USE bios_output


    IMPLICIT NONE

    REAL        :: dels  ! time step size in seconds
    INTEGER     :: kstart, kend, ktau, ktauday, counter, YYYY
    INTEGER     :: CurYear, YearStart, YearEnd ! MMY
    INTEGER     :: LOY
    CHARACTER(4):: CurYear_CHAR

    REAL(sp),DIMENSION(:),ALLOCATABLE :: data_temp
    INTEGER(i4b)                      :: iunit


    ! input rain file path
    CHARACTER(LEN = 200)   :: rain_path

    ! output nc file name
    CHARACTER(LEN = 200)   :: Rainf_name, Snow_name, LWdown_name, &
                              SWdown_name, Tair_name, Wind_name,  &
                              Qair_name, PSurf_name

    TYPE(WEATHER_GENERATOR_TYPE),SAVE :: WG
    TYPE(FILE_NAME),SAVE              :: filename


! ************ 1. Initialise variable, arrays to store things, etc *************

    dels      = 10800.  ! It should be 3 hours = 3600*3. dels is time step size
                        ! in seconds given by bios.nml
    kstart    = 1
    ktauday   = INT(24.0*3600.0/dels) ! ktauday = 8

    YearStart = 2000
    YearEnd   = 2000 ! 2017
    CurYear   = YearStart

    CALL inout_path(filename)

    rain_path    = TRIM(filename%path_in)//"/awap_rain_mm_day_2000-2017"

    CALL cable_bios_init(WG, dels, CurYear, kend, ktauday, rain_path, filename)
       ! INCLUDING:
       ! 1 CALL ReadArcFltHeader(iunit,landmaskhdr_file,MaskCols,MaskRows,MaskBndW,MaskBndS,MaskRes,NoDataVal)
       ! 2 CALL open_bios_met ! DELETED MMY
       ! 3 CALL WGEN_INIT( WG, mland, latitude, dels )

    CALL read_filename(filename)

    counter = 0

! *************************** 2. Loop over years *******************************
    DO YYYY = YearStart, YearEnd ! YYYY= CABLE_USER%YearStart,  CABLE_USER%YearEnd
       CurYear = YYYY
	     IF ((MOD(YYYY,4) == 0 .AND. MOD(YYYY,100) /= 0) .OR. MOD(YYYY,400) == 0)&
       THEN                     ! leap year
	        LOY = 366
	     ELSE
	        LOY = 365
       ENDIF

       kend = NINT(24.0*3600.0/dels) * LOY ! rounds its argument to the nearest whole number.
                                           ! kend is the total timesteps of the current year

       PRINT *,"POINT 8 CurYear, LOY, kend ", CurYear, LOY, kend ! Debug


       WRITE(CurYear_CHAR,'(I4)'), CurYear ! It is "(I4)" rather than "(A4)"
       ! PRINT *,"POINT 8.5 CurYear_CHAR ",CurYear_CHAR ! Debug

       Rainf_name   = TRIM(filename%path_out)//"/Rainf/AWAP.Rainf.3hr."//TRIM(CurYear_CHAR)//".nc"
       Snow_name    = TRIM(filename%path_out)//"/Snowf/AWAP.Snowf.3hr."//TRIM(CurYear_CHAR)//".nc"
       LWdown_name  = TRIM(filename%path_out)//"/LWdown/AWAP.LWdown.3hr."//TRIM(CurYear_CHAR)//".nc"
       SWdown_name  = TRIM(filename%path_out)//"/SWdown/AWAP.SWdown.3hr."//TRIM(CurYear_CHAR)//".nc"
       Tair_name    = TRIM(filename%path_out)//"/Tair/AWAP.Tair.3hr."//TRIM(CurYear_CHAR)//".nc"
       Wind_name    = TRIM(filename%path_out)//"/Wind/AWAP.Wind.3hr."//TRIM(CurYear_CHAR)//".nc"
       Qair_name    = TRIM(filename%path_out)//"/Qair/AWAP.Qair.3hr."//TRIM(CurYear_CHAR)//".nc"
       PSurf_name   = TRIM(filename%path_out)//"/PSurf/AWAP.PSurf.3hr."//TRIM(CurYear_CHAR)//".nc"

       PRINT *,"POINT 9 Rainf_name ", TRIM(Rainf_name) ! Debug

       CALL create_output_file(Rainf_name, "Rainf",                    &
                              "Rainfall rate",                         &
                              "rainfall_flux",                         &
                              "Rainf", "kg m-2 s-1")

       CALL create_output_file(Snow_name, "Snowf",                     &
                              "Snowfall rate",                         &
                              "snowfall_flux",                         &
                              "Snowf","kg m-2 s-1")

       CALL create_output_file(LWdown_name, "LWdown",                  &
                               "Downward Longwave Radiation",          &
                               "surface_downwelling_longwave_flux_in_air", &
                               "LWdown","W m-2")

       CALL create_output_file(SWdown_name, "SWdown",                  &
                              "Downward Shortwave Radiation",          &
                              "surface_downwelling_shortwave_flux_in_air", &
                              "SWdown", "W m-2")

       CALL create_output_file(Tair_name, "Tair",                      &
                              "Near surface air temperature",          &
                              "air_temperature",                       &
                              "Tair","K")

       CALL create_output_file(Wind_name, "Wind",                      &
                              "Near surface wind speed",               &
                              "wind_speed",                            &
                              "Wind","m s-1")

       CALL create_output_file(Qair_name, "Qair",                      &
                              "Near surface specific humidity",        &
                              "specific_humidity",                     &
                              "Qair", "kg kg-1")

       CALL create_output_file(PSurf_name, "PSurf",                    &
                              "Surface Pressure",                      &
                              "surface_air_pressure",                  &
                              "PSurf", "Pa")

       DO ktau = kstart, kend !!!!!!

          PRINT *,"POINT 11 ktau ", ktau ! Debug

          CALL cable_bios_read_met( WG, filename, counter, CurYear, YearStart, &
                                    YearEnd, ktau, kend, dels )
             ! INCLUDING:
             ! 1 CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(met%doy(1))+1 )
             ! 2 CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(met%hod(1)*3600./dels) )

          ALLOCATE(data_temp(mland))

          data_temp = WG%Precip
          CALL write_output(filename, Rainf_name, "Rainf", data_temp, dels,  &
                            CurYear, ktau, kend, .FALSE.)

          data_temp = WG%Snow
          CALL write_output(filename, Snow_name, "Snowf", data_temp, dels,   &
                            CurYear, ktau, kend, .FALSE.)

          data_temp = WG%PhiLd
          CALL write_output(filename, LWdown_name, "LWdown", data_temp, dels,&
                            CurYear, ktau, kend, .TRUE. )

          data_temp = WG%PhiSd
          CALL write_output(filename, SWdown_name, "SWdown", data_temp, dels,&
                            CurYear, ktau, kend, .TRUE. )

          data_temp = WG%Temp
          CALL write_output(filename, Tair_name, "Tair", data_temp, dels,    &
                            CurYear, ktau, kend, .FALSE.)

          data_temp = WG%Wind
          CALL write_output(filename, Wind_name, "Wind", data_temp, dels,    &
                            CurYear, ktau, kend, .TRUE. )

          data_temp = WG%QV
          CALL write_output(filename, Qair_name, "Qair", data_temp, dels,    &
                            CurYear, ktau, kend, .FALSE.)

          data_temp = WG%PPa
          CALL write_output(filename, PSurf_name, "PSurf", data_temp, dels,  &
                            CurYear, ktau, kend, .FALSE.)

          DEALLOCATE(data_temp)
          PRINT*,"POINT 17 Finish output ", CurYear, "-", ktau

       END DO ! END Do loop over timestep ktau

       PRINT*,"POINT 18 Finish translating the year of ", CurYear

    END DO !YEAR

    PRINT *,"POINT 19 Done (*^_^*) "

END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     	            	E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!
