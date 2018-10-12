MODULE cable_bios_met_obs_params

! Input routines for BIOS meteorology, remote sensing observations, and soil  
! parameters for use in the CABLE land surface scheme.
! 需要修改！！！！！！！！！！
  USE type_def_mod ! MMY
  USE bios_io_mod, ONLY: get_unit ! MMY
!  USE bios_date_functions
  
 ! USE cable_def_types_mod,   ONLY: MET_TYPE, soil_parameter_type, mland
  
  USE cable_weathergenerator,ONLY: WEATHER_GENERATOR_TYPE, WGEN_INIT, &
                                   WGEN_DAILY_CONSTANTS, WGEN_SUBDIURNAL_MET

  IMPLICIT NONE

! Define the filenames from the namelist: pathnames, landmask, daily bios .bin met files, and soil parameter files
! We define them across the whole module because they are read in by cable_bios_init but used later by
! cable_bios_load_params, i.e. outside of the bios initialisation process. We do this separately because in cable_driver  
! parameter initialisation happens after other initialisations. Our choice is to overwrite the defaults, so this has to be
! done after the defaults are read in. 

! ????? Have been defined in awap_to_netcdf.f90. Need to define them here again?
    CHARACTER(200),DIMENSION(:),ALLOCATABLE :: rain_file, swdown_file, &
                                wind_file, tairmax_file, tairmin_file, &
                                vph09_file, vph15_file ! MMY
    
    
    CHARACTER(200) :: met_path, hdr_file, & ! hdr_file MMY
                     vph09next_file, tairminnext_file ! MMY

  
  
! Define the daily bios met variables and file unit numbers which are required by multiple subroutines.
  
  REAL(sp),     ALLOCATABLE :: rain_day(:)          ! Packed vector of daily AWAP/BIOS rain (mm) 
  REAL(sp),     ALLOCATABLE :: swdown_day(:)        ! Packed vector of daily AWAP/BIOS swdown (MJ)
  REAL(sp),     ALLOCATABLE :: wind_day(:)          ! MMY
  REAL(sp),     ALLOCATABLE :: tairmax_day(:)       ! Packed vector of daily AWAP/BIOS max air temp (deg C)
  REAL(sp),     ALLOCATABLE :: tairmin_day(:)       ! Packed vector of daily AWAP/BIOS min air temp (deg C)
  REAL(sp),     ALLOCATABLE :: vph_0900(:)        ! MMY 9:00 water vapour pressure [Pa]
  REAL(sp),     ALLOCATABLE :: vph_1500(:)        ! MMY 15:00 water vapour pressure [Pa]
  REAL(sp),     ALLOCATABLE :: prev_tairmax_day(:)       ! Packed vector of previous day's AWAP/BIOS max air temp (deg C)
  REAL(sp),     ALLOCATABLE :: next_tairmin_day(:)       ! Packed vector of next day's AWAP/BIOS min air temp (deg C)
  REAL(sp),     ALLOCATABLE :: prev_vph_1500(:)     
  REAL(sp),     ALLOCATABLE :: next_vph_0900(:)      
  
  
  INTEGER(i4b),  SAVE :: rain_unit, swdown_unit, wind_unit, tairmax_unit, tairmin_unit, & ! Met file unit numbers
                         vph09_unit,vph15_unit,tairminnext_unit,vph09next_unit ! MMY
  
 ??? TYPE(dmydate), SAVE :: previous_date ! The day before the current date, for noting changes of year
!  TYPE(dmydate), SAVE :: bios_rundate  ! The day before the current date, for noting changes of year
!  TYPE(dmydate)       :: dummydate     ! Dummy date for when keeping the date is not required
!  TYPE(dmydate),SAVE  :: MetDate       ! Date of met to access (equals current date for normals runs, but
                                       ! must be calculated for spinup and initialisation runs (for dates before 1900)

  REAL(sp), PRIVATE, PARAMETER :: SecDay = 86400.
  
  TYPE(WEATHER_GENERATOR_TYPE), SAVE :: WG
 ! character(200)      :: rain_file, swdown_file, wind_file, tairmax_file, tairmin_file, tairminnext_file
    
  
CONTAINS

! ******************************** cable_bios_init ********************************
! CALL cable_bios_init(dels,curyear,met,kend,ktauday)
SUBROUTINE cable_bios_init(dels,curyear,kend,ktauday)
!CALL cable_bios_init(dels,CurYear,kend,ktauday)

USE bios_io_mod, ONLY: get_unit, ReadArcFltHeader

IMPLICIT NONE

! *************** MMY *****************
  ! From cable_IO_vars_module in cable_iovars.f90
   REAL, POINTER,DIMENSION(:) :: latitude, longitude  ! Vectors for lat and long of each land cell
   INTEGER,POINTER,DIMENSION(:) :: land_x,land_y      ! indicies of land in mask
   REAL,POINTER, DIMENSION(:,:) :: lat_all, lon_all ! lat and lon

! *************************************
   REAL, INTENT(INOUT) :: dels  
   INTEGER, INTENT(INOUT) :: curyear, kend
   INTEGER, INTENT(IN) :: ktauday

! Local variables
   LOGICAL,  SAVE :: call1 = .TRUE.
   INTEGER(i4b)   :: iunit
   INTEGER(i4b)   :: MaskCols, MaskRows  ! Landmask col and row dimensions
   REAL(sp)       :: MaskBndW, MaskBndS  ! Landmask outer bound dimensions in decimal degrees (West & South)
   REAL(sp)       :: MaskCtrW, MaskCtrS
   REAL(sp)       :: MaskRes, NoDataVal  ! Landmask resolution (dec deg) and no-data value
   
   INTEGER(i4b)   :: icol, irow, iland   ! Loop counters for cols, rows, land cells
   INTEGER(i4b), ALLOCATABLE :: ColRowGrid(:,:)      ! Temp grid to hold col or row numbers for packing to land_x or land_y

! ******************* MMY *************************
?????   TYPE(dmydate)  :: startdate,enddate  ! First and last run dates specified by user (read from cable_user%)
          !startdate= (1,1,2000),&
          !enddate  = (31,12,2017)
!TYPE(dmydate)  :: previous_date ! The day before the current date, for noting changes of year

   hdr_file =  "/short/dt6/mm3972/data/awap_rad_MJ_day_2000-2017/2017/bom-rad-day-20170702-20170702.hdr"

   CALL GET_UNIT(iunit)
   CALL ReadArcFltHeader(iunit,hdr_file,MaskCols,MaskRows,MaskBndW, &
											MaskBndS,MaskRes,NoDataVal)
! landmaskhdr_file is from bios.nml
! MaskCols, MaskRows  ! Landmask col and row dimensions
! MaskBndW, MaskBndS  ! Landmask outer bound dimensions in decimal degrees (West & South)
! MaskRes, NoDataVal  ! Landmask resolution (dec deg) and no-data value

   mland = MaskCols*MaskRows ! defined in MODULE type_def_mod
                             ! mland: CABLE's count of land points

! Allocate vectors for supplying the lat/long and col/row pairs of each
! land cell to cable.
   ALLOCATE( latitude(mland), longitude(mland) )
   ALLOCATE( land_y  (mland), land_x   (mland) )
   ALLOCATE (lat_all(MaskCols,MaskRows))
   ALLOCATE (lon_all(MaskCols,MaskRows))

   ! Populate a temporary integer grid with column numbers, then row numbers,
! packing them with the landmask into the cable vectors that record the
! column and row numbers of the land cells.
   ALLOCATE (ColRowGrid(MaskCols,MaskRows))
   FORALL (icol=1:MaskCols) ColRowGrid(icol,:) = icol
   land_x = PACK(ColRowGrid,.true.)
   FORALL (irow=1:MaskRows) ColRowGrid(:,irow) = irow
   land_y = PACK(ColRowGrid,.true.)
   
???? if the pack function the same as the data read in way? If are the data mismatched???


! Using the landmask grid boundaries and dimensions, translate the land
! cell cols and rows of land_x and land_y into corresponding lats and longs.
   MaskCtrW = MaskBndW + (MaskRes / 2.0) ! Convert western and southern
   MaskCtrS = MaskBndS + (MaskRes / 2.0) ! boundaries to cell centres.
   DO iLand = 1,mland
	  longitude(iLand) = MaskRes * real((land_x(iLand) - 1)) + MaskCtrW   
	  latitude(iLand) = MaskCtrS + (real(MaskRows - land_y(iLand)) * MaskRes)
   END DO
       !MaskCols 列  MaskRows 行
 ! convert latitude and longitude to array
  lat_all = UNPACK(latitude,.true.,field=-9999.)
  lon_all = UNPACK(longitude,.true.,field=-9999.)

! Finished reading grids. Only mland vectors from now on.
   DEALLOCATE (ColRowGrid)
   DEALLOCATE (land_x)
   DEALLOCATE (land_y)
! If this is the first ever initialisation, open the met files, sort
! out the run's date range with respect to the available met data,
! and allocate memory for met vars. Otherwise, just rewind the
! already-open met files.
IF (call1) then

	! CALL open_bios_met ! MMY
	ALLOCATE (   rain_day(mland))
	ALLOCATE ( swdown_day(mland))
	ALLOCATE (   wind_day(mland)) ! MMY
	ALLOCATE (tairmax_day(mland))
	ALLOCATE (tairmin_day(mland))
	ALLOCATE (   vph_0900(mland))
	ALLOCATE (   vph_1500(mland))
    ALLOCATE (prev_tairmax_day(mland))
	ALLOCATE (next_tairmin_day(mland))
    ALLOCATE (prev_vph_1500(mland))
	ALLOCATE (next_vph_0900(mland))
 
!	curyear = 2000  ! The initial current year is set prior to any skipping.

! Initialise Weather Generator
	CALL WGEN_INIT( WG, mland, latitude, dels )
! SUBROUTINE WGEN_INIT( WG, np, latitude, dels )

! Record that the first call is now over.
	call1 = .FALSE.

END IF

END SUBROUTINE cable_bios_init
! 11:13
! ******************************* cable_bios_read_met ******************************
SUBROUTINE cable_bios_read_met( counter, CurYear, ktau, kend, islast, dels )
! CALL  cable_bios_read_met(MET, CurYear, ktau, kend,(YYYY.EQ.CABLE_USER%YearEnd .AND. ktau.EQ.kend), dels )

	! Read a single day of meteorology from all bios met files, updating the bios_rundate
	! If a change of year has occurred, read an annual CO2 record

	IMPLICIT NONE

	INTEGER, INTENT(IN)  :: CurYear, ktau, kend
	LOGICAL, INTENT(IN)  :: islast
	REAL, INTENT(IN) :: dels              ! NOTE: dels should be 3h once, time step size in seconds
!	TYPE(MET_TYPE), INTENT(INOUT)       :: met

	LOGICAL(lgt)   :: newday
	real(sp),parameter:: RMWbyRMA  = 0.62188471 ! molecular wt of water [kg/mol] / atomic wt of C [kg/mol]
	integer(i4b)   :: iland       ! Loop counter through mland land cells
    real           :: hod, doy, year
    LOGICAL(lgt)   :: error_status
    ! **************************************************
    ! CABLE is calculated in every grid's tile,and landpt(:)%cstart is the position of 1st gridcell veg patch in main arrays
    ! but in weathergenerator we don't need to consider the veg patch, and the smallest unit shoulb be grid.
	hod  = REAL(MOD( (ktau-1) * NINT(dels), INT(SecDay)) ) / 3600. !(landpt(:)%cstart)
	doy  = INT(REAL(ktau-1) * dels / SecDay ) + 1                  !(landpt(:)%cstart)
	year = Curyear                                                 !(landpt(:)%cstart)

	newday = ( hod .EQ. 0 )                                        !landpt(1)%cstart

IF ( newday ) THEN
    counter = counter + 1 
! ************************** MMY **********************************
    !met_path = " /"+ year + "/"  make rain_file include the path
    
	CALL GET_UNIT(rain_unit)  ! Rainfall
	OPEN (rain_unit, FILE=TRIM(rain_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (rain_unit) rain_day          ! Packed vector of daily AWAP/BIOS rain (mm) ! romove bios_rundate, 
	CLOSE(rain_unit)
    
	CALL GET_UNIT(swdown_unit)  ! Shortwave downward solar radiation
	OPEN (swdown_unit, FILE=TRIM(swdown_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (swdown_unit) swdown_day        ! Packed vector of daily AWAP/BIOS swdown (MJ) ! romove bios_rundate, 
    CLOSE(swdown_unit)
    
	CALL GET_UNIT(wind_unit)  ! Wind MMY
	OPEN (wind_unit, FILE=TRIM(wind_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (wind_unit) wind_day            ! MMY
    CLOSE (wind_unit) 
	
	CALL GET_UNIT(tairmax_unit)  ! Maximum air temperature
	OPEN (tairmax_unit, FILE=TRIM(tairmax_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (tairmax_unit) tairmax_day       ! Packed vector of daily AWAP/BIOS max air temp (deg C) ! romove bios_rundate, 
	CLOSE (tairmax_unit)
    
	CALL GET_UNIT(tairmin_unit)  ! Minimum air temperature
	OPEN (tairmin_unit, FILE=TRIM(tairmin_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (tairmin_unit) tairmin_day       ! Packed vector of daily AWAP/BIOS min air temp (deg C) ! romove bios_rundate, 
    CLOSE (tairmin_unit)
    
    CALL GET_UNIT(vph09_unit)  
	OPEN (vph09_unit, FILE=TRIM(vph09_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (vph09_unit) vph_0900       
    CLOSE (vph09_unit)
    
    CALL GET_UNIT(vph15_unit)  
	OPEN (vph15_unit, FILE=TRIM(vph15_file(counter)), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
    READ (vph15_unit) vph_1500       
    CLOSE (vph15_unit)
    
    ! ************************************************************************************************************
        
    WG%TempMaxDayPrev = WG%TempMaxDay   !!!!! note: do not lose the previous WG data during loop !!!!!!!!!!
    WG%VapPPa1500Prev = WG%VapPPa1500
    
    !**************** MMY *****************
    if (ktau .ne. kend .and. CurYear .ne. YearEnd) then
       CALL GET_UNIT(tairminnext_unit)  ! Minimum air temperature
       CALL GET_UNIT(vph09next_unit)

       tairminnext_file = tairmin_file(counter+1)
       vph15next_file   = vph15_file(counter+1)

	   OPEN (tairminnext_unit, FILE=TRIM(tairminnext_file), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
	   READ (tairminnext_unit) next_tairmin_day      ! Packed vector of daily AWAP/BIOS min air temp (deg C)
       CLOSE(tairminnext_unit)
       
	   OPEN (vph09next_unit, FILE=TRIM(vph09next_file), FORM='BINARY', STATUS='OLD',IOSTAT=error_status)
	   READ (vph09next_unit) next_vph_0900      
       CLOSE(vph09next_unit)
       
    else
       next_tairmin_day = tairmin_day
       next_vph_0900    = vph_0900
    endif
    
    !***************************************
       
    WG%WindDay        = wind_day !MMY  2.0 ! fixed value, pending ingestion of McVicar data set
	WG%TempMinDay     = tairmin_day
	WG%TempMaxDay     = tairmax_day
	WG%VapPPa0900     = vph_0900
	WG%VapPPa1500     = vph_1500
    
    !**************** MMY *****************
    if (ktau .eq. 1 .and. CurYear .eq. YearStart ) then
    WG%TempMaxDayPrev = tairmax_day
    WG%VapPPa1500Prev = vph_1500
    end if 
    !**************************************
		 
    WG%TempMinDayNext = next_tairmin_day 
    WG%VapPPa0900Next = next_vph_0900
    
	!WG%VapPmbDay = esatf(tairmin_day)

	WG%SolarMJDay     = swdown_day
	WG%PrecipDay      = rain_day / 1000. ! ->[m/d]
	where ( WG%TempMinDay .lt. -2.0)
		WG%SnowDay        =  rain_day / 1000. ! ->[m/d]
		WG%PrecipDay      = 0.0
	elsewhere
		WG%SnowDay        = 0.0
	end where

	WG%PmbDay = 1000.0 ! Air pressure in mb fixed in space and time

	CALL WGEN_DAILY_CONSTANTS( WG, mland, INT(doy)+1 )
         ! SUBROUTINE WGEN_DAILY_CONSTANTS( WG, np, YearDay )
END IF


! follow one file such rain to go through the program and find every procedure needed for tranlating the data
! and can chance the data format by using array instead of ALLOCATE
! when finish follow the file and processes I will know which parts are lost and which parts are unneceserity
    CALL WGEN_SUBDIURNAL_MET( WG, mland, NINT(hod*3600./dels) )
! SUBROUTINE WGEN_SUBDIURNAL_MET(WG, np, itime)

!*******************************************************************************
CONTAINS
ELEMENTAL FUNCTION Esatf(TC)
!-------------------------------------------------------------------------------
! At temperature TC [deg C], return saturation water vapour pressure Esatf [mb]
! from Teten formula.
! MRR, xx/1987
! PRB, 09/1999:   Convert to F95 elemental function; works on scalars and arrays
!                 just like intrinsic functions.
! MRR, 12-mar-02: Convert Qsatf (specific humidity routine) to Esatf
!-------------------------------------------------------------------------------
implicit none
real(sp), intent(in):: TC           ! temp [deg C]
real(sp):: Esatf                    ! saturation vapour pressure [mb]
real(sp):: TCtmp                    ! local
real(sp),parameter:: A = 6.106      ! Teten coefficients
real(sp),parameter:: B = 17.27      ! Teten coefficients
real(sp),parameter:: C = 237.3      ! Teten coefficients
!-------------------------------------------------------------------------------
TCtmp = TC                          ! preserve TC
if (TCtmp.gt.100.0) TCtmp = 100.0   ! constrain TC to (-40.0,100.0)
if (TCtmp.lt.-40.0) TCtmp = -40.0
Esatf = A*EXP(B*TCtmp/(C+TCtmp))    ! sat vapour pressure (mb)

END FUNCTION Esatf

!*******************************************************************************

END SUBROUTINE cable_bios_read_met


!********************************** MMY ************************************
SUBROUTINE cable_bios_write_met 
	
    IMPLICIT NONE








! 这里直接设置output  不需要met了

 WG%Precip(:) ! Rainf/GSWP3.BC.Rainf.3hrMap ! precip [mm/h]
 WG%Snow(:)   ! Snowf/GSWP3.BC.Snowf.3hrMap ! precip [mm/h]
 WG%PhiLD(iland) ! LWdown/GSWP3.BC.LWdown.3hrMap ! down longwave irradiance  [W/m2]
 WG%PhiSD(iland) ! SWdown/GSWP3.BC.SWdown.3hrMap ! downward solar irradiance [W/m2]
 WG%Temp(:) = WG%Temp(:) + 273.15
 WG%Wind(iland)
 WG%QV    = WG%VapPmb(iland)/WG%Pmb(iland)*RMWbyRMA ! Qair/GSWP3.BC.Qair.3hrMap ! specific humidity (kg/kg) 
 WG%Pmb   = WG%Pmb * 100 ! PSurf/GSWP3.BC.PSurf.3hrMap ! pressure 1 [mb] = 100 [Pa]
 
 
END SUBROUTINE cable_bios_write_met

END MODULE cable_bios_met_obs_params

! *************************************************************************************
