!--------------------------------------------------------------------------!
!		     		M A I N   P R O G R A M             	               !
!--------------------------------------------------------------------------!
PROGRAM awap_to_netcdf

    USE type_def_mod
    USE bios_io_mod, ONLY: get_unit ! MMY
	USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
									WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET
    USE

	IMPLICIT NONE

	
    REAL, INTENT(INOUT) :: dels  ! time step size in seconds
    INTEGER, INTENT(INOUT) ::  kstart, kend ,ktauday, counter
  
    LOGICAL,  SAVE :: call1 = .TRUE.
    INTEGER(i4b)   :: iunit  
  
! *********************** MMY ***************************
    
    INTEGER, INTENT(INOUT) ::CurYear, YearStart, YearEnd
    CHARACTER(200),DIMENSION(:),ALLOCATABLE :: rain_file, swdown_file, &
                                wind_file, tairmax_file, tairmin_file, &
                                vph09_file, vph15_file ! MMY
    用指针
     ？？？ 动态数组和,save 之间关系

! *******************************************************

  ! 1. Initialise variable, arrays to store things, etc 
  
    dels      = 10800.     !it should be 3 hours = 3600*3  dels is time step size in seconds and is original from dels in bios.nml 
    kstart    = 1
    ktauday   = INT(24.0*3600.0/dels) !ktauday = 8
        
    YearStart = 2000
    YearEnd   = 2017 
    CurYear   = 2000
    
    met_path     = "/short/dt6/mm3972/data/AWAP_data"
    rain_path    = "/awap_rain_mm_day_2000-2017"
    swdown_path  = "/awap_rad_MJ_day_2000-2017"
    wind_path    = "/mcvicar_windspeed_ms_day_2000-2017"
    tairmax_path = "/awap_tmax_C_day_2000-2017"
    tairmin_path = "/awap_tmin_C_day_2000-2017"
    vph09_path   = "/awap_vph09_hpa_day_2000-2017"
    vph15_path   = "/awap_vph15_hpa_day_2000-2017"
       
    CALL cable_bios_init(dels,CurYear,kend,ktauday) !dels in bios.nml ! remove met by MMY
       ! met is not given values in cable_bios_init subroutine
       ! cable_bios_init is to:
       ! 1 CALL ReadArcFltHeader(iunit,landmaskhdr_file,MaskCols,MaskRows,MaskBndW,MaskBndS,MaskRes,NoDataVal)
       ! 2 CALL open_bios_met ! CANCELED BY MMY
       ! 3 CALL WGEN_INIT( WG, mland, latitude, dels )
  
    CALL read_filename(met_path,rain_path,rain_file)
    CALL read_filename(met_path,swdown_path,swdown_file)
    CALL read_filename(met_path,wind_path,wind_file)
    CALL read_filename(met_path,tairmax_path,tairmax_file)
    CALL read_filename(met_path,tairmin_path,tairmin_file)
    CALL read_filename(met_path,vph09_path,vph09_file)
    CALL read_filename(met_path,vph15_path,vph15_file)
    
    counter = 0

	! 2. Loop over years
YEAR: DO YYYY = YearStart, YearEnd ! YYYY= CABLE_USER%YearStart,  CABLE_USER%YearEnd
	   CurYear = YYYY
	   IF (( MOD( YYYY,  4 ) .EQ. 0 .AND. MOD( YYYY, 100 ) .NE. 0 ) .OR. MOD( YYYY,400 ) .EQ. 0 )   THEN ! whether the current year is leap year
	      LOY = 366
	   ELSE
	      LOY = 365
       ENDIF

       kend = NINT(24.0*3600.0/dels) * LOY ! rounds its argument to the nearest whole number.
                                           ! kend is the total timesteps of the current year
       
       met_file_out = "/short/dt6/mm3972/data/AWAP_to_netcdf/"
       Rainf_name   = "Rainf/AWAP.Rainf.3hr."//CurYear//".nc"
       Snow_name    = "Snowf/AWAP.Snowf.3hr."//CurYear//".nc"
       LWdown_name  = "LWdown/AWAP.LWdown.3hr."//CurYear//".nc"
       SWdown_name  = "SWdown/AWAP.SWdown.3hr."//CurYear//".nc"
       Tair_name    = "Tair/AWAP.Tair.3hr."//CurYear//".nc"
       Wind_name    = "Wind/AWAP.Wind.3hr."//CurYear//".nc"
       Qair_name    = "Qair/AWAP.Qair.3hr."//CurYear//".nc"
       PSurf_name   = "PSurf/AWAP.PSurf.3hr."//CurYear//".nc"
       
       
       
       
 
DO ktau = kstart, kend
    
 	CALL  cable_bios_read_met(counter, CurYear, ktau, kend,(YYYY .EQ. YearEnd .AND. ktau .EQ. kend), dels )
        ! SUBROUTINE cable_bios_read_met(MET, CurYear, ktau, kend, islast, dels )
				! CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(met%doy(1))+1 )
				! CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(met%hod(1)*3600./dels) )
   


    



END DO ! END Do loop over timestep ktau
END DO YEAR

! **************************************** 输 出 ************************************

    

      ! 这里直接设置output  不需要met了

 WG%Precip(:) ! Rainf/GSWP3.BC.Rainf.3hrMap ! precip [mm/h]
 WG%Snow(:)   ! Snowf/GSWP3.BC.Snowf.3hrMap ! precip [mm/h]
 WG%PhiLD(iland) ! LWdown/GSWP3.BC.LWdown.3hrMap ! down longwave irradiance  [W/m2]
 WG%PhiSD(iland) ! SWdown/GSWP3.BC.SWdown.3hrMap ! downward solar irradiance [W/m2]
 WG%Temp(:) = WG%Temp(:) + 273.15  ! Tair/GSWP3.BC.Tair.3hrMap
 WG%Wind(iland) ! Wind/GSWP3.BC.Wind.3hrMap
 ! WG%QV needed to be defined
 WG%QV    = WG%VapPmb(iland)/WG%Pmb(iland)*RMWbyRMA ! Qair/GSWP3.BC.Qair.3hrMap ! specific humidity (kg/kg) 
 WG%Pmb   = WG%Pmb * 100 ! PSurf/GSWP3.BC.PSurf.3hrMap ! pressure 1 [mb] = 100 [Pa]
 
 

 















	! Read header file
	! Open AWAP file for each variable and fill up arrays.
	! loop over years, days
	! open AWAP file
	! call weather generator
	! write netcdf file




END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     		E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!







    




