MODULE bios_io_mod !    MMY
! ******************************************************************************
! USAGE:
! INCLUDE:
! ******************************************************************************

    USE IFPORT  ! To use systemqq, the module is needed.
    USE type_def_mod, ONLY: i4b, sp, FILE_NAME
 
  CONTAINS

! ======================= From MGK and modified by MMY =========================
  SUBROUTINE inout_path(filename)

      IMPLICIT NONE

      CHARACTER(LEN = 200) :: arg
      TYPE(FILE_NAME)      :: filename

      IF ( IARGC() > 0 ) THEN

         CALL GETARG(1, arg)
         !   ??????????? arg(1:2) ???????????
         IF (arg(1:2) == '-u' .OR. arg(1:2) == '-h') THEN
            WRITE (*, *) '================== USAGE =================='
            WRITE (*, *) 'e.g...             '
            WRITE (*, *) '                   '
            WRITE (*, *) 'Makefile /short/dt6/mm3972/data/AWAP_data &
                          /short/w35/mm3972/data/AWAP_to_netcdf'
            WRITE (*, *) '                                           '
            STOP
         ELSE
            CALL GETARG(1, filename%path_in)
            CALL GETARG(2, filename%path_out)
         ENDIF

      ELSE

         filename%path_in  = "/short/dt6/mm3972/data/AWAP_data"
         filename%path_out = "/short/w35/mm3972/data/AWAP_to_netcdf"

      END IF

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
  SUBROUTINE read_filename(file_path, file_name)

      IMPLICIT NONE

      INTEGER        :: iunit, ok, file_num
      CHARACTER(200) :: file_path
      CHARACTER(200),DIMENSION(:),ALLOCATABLE :: file_name
      CHARACTER(500) :: commandline
  
   ! read file name
      
      commandline = 'find '//TRIM(file_path)//' -name "*.flt" -fprintf ./temp.txt "%p\n"'
      ! output the absolute path of met files
      print*,commandline
      ok = systemqq(commandline)

      commandline = 'sort -n <./temp.txt >./namelist.txt'
      print*,commandline
      ok = systemqq(commandline)

      ! ??? the finding cannot pass to sort command, I dont know why
      ! ??? is it because I used system which uses different environment
      ! ??? but systemqq uses the same environment

      CALL GET_UNIT(iunit)
      OPEN (iunit, file="filenum.txt",status="old",action="read")
      READ (iunit, *) file_num
      CLOSE(iunit)

      ALLOCATE(file_name(file_num))
      
      CALL GET_UNIT(iunit)
      OPEN (iunit, file="namelist.txt",status="old",action="read")
      READ (iunit, *) file_name
      CLOSE(iunit)
      PRINT *, 'POINT 1 input filenames ',file_name ! Debug
      ok = systemqq('rm temp.txt')
      !ok = systemqq('rm namelist.txt')

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
      PRINT *,"Point 2 Headfile:",Headfile,Cols,Rows,xLL,yLL,CellSize,NoDataVal

  END SUBROUTINE ReadArcFltHeader

END MODULE bios_io_mod
