MODULE bios_output
! ******************************************************************************
! USAGE:
! INCLUDE:
! ******************************************************************************

    USE netcdf

    IMPLICIT NONE

    INTEGER :: ok   ! netcdf error status
 
  CONTAINS

  SUBROUTINE create_output_file(fileout,ncid_out,varID,tvar,vname,longname,&
                                standardname, almaname, vunits)
    ! Creates netcdf output file, defines variables

      USE cable_bios_met_obs_params, ONLY: MaskCols, MaskRows, &
                                           MaskCtrW, MaskCtrS, &
                                           MaskRes, NoDataVal
      IMPLICIT NONE

      CHARACTER(LEN=200),INTENT(IN):: fileout

      INTEGER, INTENT(OUT) :: ncid_out ! output data netcdf file ID
      INTEGER, INTENT(OUT) :: varID, tvar ! var,time ID

      CHARACTER(LEN=*), INTENT(IN) :: vname ! name of variable
      CHARACTER(LEN=*), INTENT(IN) :: longname ! full variable name
      CHARACTER(LEN=*), INTENT(IN) :: standardname ! standard variable name
      CHARACTER(LEN=*), INTENT(IN) :: almaname !alma variable name
      CHARACTER(LEN=*), INTENT(IN) :: vunits ! variable units

      INTEGER :: latID, lonID  ! lat, lon ID
      INTEGER :: tID, xID, yID ! dimension ID
      INTEGER :: x, y
      REAL,ALLOCATABLE :: lat_val(:),lon_val(:)


    ! Create output file:
      ok = NF90_CREATE(TRIM(fileout), NF90_CLOBBER, ncid_out)

    ! Put the file in define mode:
      ok = NF90_REDEF(ncid_out)

    ! Define dimensions:
      ok = NF90_DEF_DIM(ncid_out, 'time', NF90_UNLIMITED, tID)
      ok = NF90_DEF_DIM(ncid_out, 'lat',  MaskRows, xID) ! xdimsize
      ok = NF90_DEF_DIM(ncid_out, 'lon',  MaskCols, yID) ! ydimsize

    ! Define "time" variable and its attributes:
      ok = NF90_DEF_VAR(ncid_out, 'time', NF90_DOUBLE, (/tID/), tvar)
      ok = NF90_PUT_ATT(ncid_out, tvar, 'long_name', "Time")
      ok = NF90_PUT_ATT(ncid_out, tvar, 'standard_name', "time")
      ok = NF90_PUT_ATT(ncid_out, tvar, 'calendar', "proleptic_gregorian")
      ok = NF90_PUT_ATT(ncid_out, tvar, 'units', "hours since 1901-01-01 &
             00:00:00")
    !!!!!! "proleptic_gregorian" is from GWSP

    ! Define latitude and longitude variable (ALMA):
      ok = NF90_DEF_VAR(ncid_out, 'lat', NF90_FLOAT, (/xID/), latID)
      ok = NF90_PUT_ATT(ncid_out, latID, 'long_name', "Latitude")
      ok = NF90_PUT_ATT(ncid_out, latID, 'standard_name', "latitude")
      ok = NF90_PUT_ATT(ncid_out, latID, 'axis', "Y")
      ok = NF90_PUT_ATT(ncid_out, latID, 'units', "degrees_north")

      ok = NF90_DEF_VAR(ncid_out, 'lon', NF90_FLOAT, (/yID/), lonID)
      ok = NF90_PUT_ATT(ncid_out, lonID, 'long_name', "Longitude")
      ok = NF90_PUT_ATT(ncid_out, lonID, 'standard_name', "longitude")
      ok = NF90_PUT_ATT(ncid_out, lonID, 'axis', "X")
      ok = NF90_PUT_ATT(ncid_out, lonID, 'units', "degrees_east")

    ! Define variable:
      ok = NF90_DEF_VAR(ncid_out, vname, NF90_FLOAT, (/tID, xID, yID/), varID)
      ok = NF90_PUT_ATT(ncid_out, varID, 'long_name', longname)
      ok = NF90_PUT_ATT(ncid_out, varID, 'standard_name', standardname)
      ok = NF90_PUT_ATT(ncid_out, varID, 'alma_name', almaname)
      ok = NF90_PUT_ATT(ncid_out, varID, 'units', vunits)
      ok = NF90_PUT_ATT(ncid_out, varID, '_fillvalue', NoDataVal) !!!! GWSP 1.e+20 AWAP -999

    ! End netcdf define mode:
      ok = NF90_ENDDEF(ncid_out)

      ALLOCATE (lat_val(MaskRows))
      ALLOCATE (lon_val(MaskCols))

    ! in GWSP nc file lat is from south to north and lon is from 0 to 180 to 0
      DO x = 1, MaskRows
         lat_val(x) = MaskCtrS + MaskRes * ( x - 1 )
      END DO

      !PRINT *, "POINT 3 lat_val ",lat_val ! Debug

      DO y = 1, MaskCols
         lon_val(y) = MaskCtrW + MaskRes * ( y - 1 )
      END DO

      !PRINT *, "POINT 4 lon_val ",lon_val ! Debug

      ! Write latitude and longitude variables:
      ok = NF90_PUT_VAR(ncid_out, latID, REAL(lat_val, 4))
      ok = NF90_PUT_VAR(ncid_out, lonID, REAL(lon_val, 4))

      DEALLOCATE (lat_val)
      DEALLOCATE (lon_val)

  END SUBROUTINE create_output_file



! ==================================== MMY =====================================
  SUBROUTINE write_output(filename, met_1D, dels, CurYear, ktau, kend, ocnmask,&
                          ncid_out, varID, tvar)
    ! Writes model output variables and, if requested, calls
    ! energy and mass balance routines. This subroutine is called
    ! each timestep, but may only write to the output file periodically,
    ! depending on whether the user has specified that output should be
    ! aggregated, e.g. to monthly or 6-hourly averages.
      
      USE type_def_mod, ONLY: mland, file_name,i4b,r_2,sp
      USE bios_io_mod, ONLY: get_unit
      USE cable_bios_met_obs_params, ONLY: MaskCols, MaskRows

      IMPLICIT NONE

      REAL, INTENT(IN)    :: dels
      INTEGER, INTENT(IN) :: CurYear, ktau, kend ! timestep number in loop which include spinup
      LOGICAL, INTENT(IN) :: ocnmask ! whether it is default value over ocean
      INTEGER, INTENT(IN) :: ncid_out, varID, tvar

      REAL                :: delh ! time step size in hour
      INTEGER             :: rows
      INTEGER(i4b)        :: iunit
      REAL(r_2), DIMENSION(1) :: timetemp ! temporary variable for storing time

      REAL(sp),DIMENSION(:),ALLOCATABLE   :: met_1D
      REAL(sp),DIMENSION(:,:),ALLOCATABLE :: met_2D, met_2D_temp, mask_value
      LOGICAL,DIMENSION(:,:),ALLOCATABLE  :: mask_2D

      TYPE(FILE_NAME)     :: filename


      ! ALLOCATE(met_1D(mland)                  )
      ALLOCATE(met_2D(MaskRows, MaskCols)     )
      ALLOCATE(met_2D_temp(MaskCols, MaskRows))
      ALLOCATE(mask_2D(MaskCols, MaskRows)    )
      ALLOCATE(mask_value(MaskRows, MaskCols) )
      
      PRINT *,"POINT 13 ALLOCATE"

      met_2D_temp = -999.

      CALL GET_UNIT(iunit)
      OPEN (iunit, file=TRIM(filename%swdown_file(1)), FORM='BINARY', &
           status="old",action="read")
      READ(iunit) mask_value
      CLOSE(iunit) 

      PRINT *,"POINT 14 SWDOWN mask_value"

      IF (ocnmask) THEN
         WHERE ( mask_value == -999.)
             mask_2D = .FALSE.
         ELSEWHERE
             mask_2D = .TRUE.
         END WHERE
      ELSE
         mask_2D = .TRUE.
      END IF

      met_2D_temp  = UNPACK(met_1D, mask_2D, met_2D_temp)
      
      ! Debug: need to check the map of default value distribution


    ! reverse lat
      DO rows = 1, MaskRows
         met_2D(rows,:) = met_2D_temp(:, MaskRows + 1 - rows)
         ! TRICK, FORTRAN processes in a column priority way
      END DO
       
      PRINT *,"met_1D", met_1D

      !!!!!!!! WIND & RADIATION OVER OCEAN ARE DEFAULT value
      ! check whether the longwave and other var are default over ocean

      ! DEALLOCATE(met_1D     )
      DEALLOCATE(mask_2D    )
      DEALLOCATE(mask_value )
      DEALLOCATE(met_2D_temp)
      
      PRINT *,"POINT 15 DEALLOCATE"

      delh = dels/3600.
    ! Write to temporary time variable:
    ! hours from 1901-01-01-00:00
      IF (CurYear >= 1901) THEN
         timetemp = DBLE((INT((CurYear - 1991)/4.)*(366+365*3) &
                    + MOD((CurYear - 1991),4)*365)*24.         &
                    + (ktau-1)*delh)
      ELSE
         PRINT *, "ERROR: CurYear should be larger than 1990"
      END IF

    ! Write time variable for this output time step:
      ok = NF90_PUT_VAR(ncid_out, tvar, timetemp, start = (/ktau/), count = (/1/))
    ! timetemp shouldn't accumulate

    ! From write_output_variable_r1 in cable_write.F90
      ok = NF90_PUT_VAR(ncid_out, varID, REAL(met_2D, 4),start = (/ktau,1,1/),&
           count = (/1, MaskRows, MaskCols/)) ! write data to file
     ! PRINT *,"POINT 15.5 met_2D",met_2D
      DEALLOCATE( met_2D )


  END SUBROUTINE write_output


END MODULE bios_output
