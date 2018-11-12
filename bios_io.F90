MODULE bios_io_mod !    MMY
! ******************************************************************************
! USAGE: Prepare the input files and output files
! INCLUDE: IFPORT (for using linux command), type_def_mod
! ******************************************************************************

    USE IFPORT  ! To use systemqq, the module is needed.
    USE type_def_mod, ONLY: i4b, sp, FILE_NAME

  CONTAINS

! ======================= From MGK and modified by MMY =========================
  SUBROUTINE inout_path(filename)

      IMPLICIT NONE

      INTEGER              :: ok
      CHARACTER(500)       :: commandline
      CHARACTER(LEN = 200) :: arg
      TYPE(FILE_NAME)      :: filename


      IF ( IARGC() > 0 ) THEN

         CALL GETARG(1, arg)
         !   ??? arg(1:2)
         IF (arg(1:2) == '-u' .OR. arg(1:2) == '-h') THEN
            WRITE (*, *) '================== USAGE =================='
            WRITE (*, *) 'e.g...             '
            WRITE (*, *) '                   '
            WRITE (*, *) 'Makefile /short/w35/mm3972/data/AWAP_data &
                          /short/w35/mm3972/data/AWAP_to_netcdf'
            WRITE (*, *) '                                           '
            STOP
         ELSE
            CALL GETARG(1, filename%path_in)
            CALL GETARG(2, filename%path_out)
         ENDIF

      ELSE

         filename%path_in  = "/short/w35/mm3972/data/AWAP_data"
         filename%path_out = "/short/w35/mm3972/data/AWAP_to_netcdf"

      END IF

      ! create output data file folders
      commandline = 'mkdir '//TRIM(filename%path_out)//'/Rainf'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/Snowf'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/LWdown'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/SWdown'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/Tair'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/Wind'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/Qair'
      ok = systemqq(commandline)

      commandline = 'mkdir '//TRIM(filename%path_out)//'/PSurf'
      ok = systemqq(commandline)

      PRINT *,"POINT 1 Reading input and output paths"

  END SUBROUTINE inout_path

! ================== From ./core/biogeophys/cable_common.F90 ===================

  SUBROUTINE get_unit(iunit)

    ! Find an unused unit to open a file.
      IMPLICIT NONE

      INTEGER, INTENT(OUT) :: iunit
      INTEGER :: i
      LOGICAL :: is_open = .FALSE.

      DO i = 200, 10000
         INQUIRE ( UNIT=i, OPENED=is_open )
         IF ( .NOT. is_open ) EXIT
      END DO
      iunit = i

  END SUBROUTINE get_unit

! =================================== MMY ======================================
  SUBROUTINE read_filename(filename)

      IMPLICIT NONE

      INTEGER        :: iunit, file_num, i, ok
      CHARACTER(500) :: commandline
      TYPE(FILE_NAME):: filename

      CALL GET_UNIT(iunit)
      OPEN (iunit, file="filenum.txt",status="old",action="read")
      READ (iunit, *) file_num
      CLOSE(iunit)
      ok = systemqq('rm ./filenum.txt')

   ! READING MET DATA
      ! rain
      commandline = 'find '//TRIM(filename%path_in)//\
          '/awap_rain_mm_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_rain.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_rain.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%rain_file(i)
      END DO
      CLOSE(iunit)

      ! rad
      commandline = 'find '//TRIM(filename%path_in)//\
           '/awap_rad_MJ_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_rad.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_rad.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%swdown_file(i)
      END DO
      CLOSE(iunit)

      ! wind
      commandline = 'find '//TRIM(filename%path_in)//\
      '/mcvicar_windspeed_ms_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_wind.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_wind.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%wind_file(i)
      END DO
      CLOSE(iunit)

      ! tmax
      commandline = 'find '//TRIM(filename%path_in)//\
           '/awap_tmax_C_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_tmax.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_tmax.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%tairmax_file(i)
      END DO
      CLOSE(iunit)

      ! tmin
      commandline = 'find '//TRIM(filename%path_in)//\
           '/awap_tmin_C_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_tmin.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_tmin.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%tairmin_file(i)
      END DO
      CLOSE(iunit)

      ! vph09
      commandline = 'find '//TRIM(filename%path_in)//\
        '/awap_vph09_hpa_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_vph09.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_vph09.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%vph09_file(i)
      END DO
      CLOSE(iunit)

      ! vph15
      commandline = 'find '//TRIM(filename%path_in)//\
        '/awap_vph15_hpa_day_2000-2017 -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ok = systemqq(commandline)
      commandline = 'sort -n <./temp.txt >./namelist_vph15.txt'
      ok = systemqq(commandline)
      OPEN (iunit, file="namelist_vph15.txt",status="old",action="read")
      DO i = 1,file_num
         READ (iunit, '(A)') filename%vph15_file(i)
      END DO
      CLOSE(iunit)

      ok = systemqq('rm temp.txt')

      PRINT *, 'POINT 7 input filenames, e.g. ', TRIM(filename%rain_file(file_num)) ! Debug

  END SUBROUTINE read_filename


! ================ From ./offline/cable_bios_met_obs_params.F90 ================

  SUBROUTINE ReadArcFltHeader(iunit,Headfile,Cols,Rows,xLL,yLL,CellSize,NoDataVal)
!-------------------------------------------------------------------------------
! Read the header file (.hdr) associated with an ArcGIS binary grid file (.hdr).
! The elements are req'd in standard order, although ArcGIS is not reliant on this
! order. A seventh line is often found specifying the endianness, but this is not
! read. Return the grid dimensions and no data value.
!-------------------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER(i4b), INTENT(IN)   :: iunit
      INTEGER(i4b), INTENT(OUT)  :: Cols, Rows
      REAL(sp), INTENT(OUT)      :: xLL, yLL, CellSize, NoDataVal
      CHARACTER(200), INTENT(IN) :: Headfile
      CHARACTER(12)              :: Head

!-------------------------------------------------------------------------------
      OPEN (unit = iunit, file = Headfile, status = 'old')

      READ (iunit,*) Head, Cols      ! Number of columns
      READ (iunit,*) Head, Rows      ! Number of rows
      READ (iunit,*) Head, xLL       ! Western boundary
      READ (iunit,*) Head, yLL       ! Southern boundary
      READ (iunit,*) Head, CellSize  ! Resolution (both W-E, N-S)
      READ (iunit,*) Head, NoDataVal ! Missing data value

      CLOSE (unit=iunit)
      ! Debug
      PRINT *,"POINT 3 Headfile:  ",Headfile
      PRINT *,"MaskCols, MaskRows ",Cols,Rows
      PRINT *,"MaskBndW, MaskBndS ",xLL,yLL
      PRINT *,"CellSize,NoDataVal ",CellSize,NoDataVal

  END SUBROUTINE ReadArcFltHeader

END MODULE bios_io_mod
