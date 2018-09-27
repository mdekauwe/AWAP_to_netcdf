!--------------------------------------------------------------------------!
!		     		M A I N   P R O G R A M								               !
!--------------------------------------------------------------------------!
PROGRAM awap_to_netcdf

	IMPLICIT NONE


	! 1. Iniitalise variable, arrays to store things, etc
	! 2. Loop over years
	! Read header file
	! Open AWAP file for each variable and fill up arrays.
	! loop over years, days
	! open AWAP file
	! call weather generator
	! write netcdf file




	CALL GET_UNIT(iunit)




END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     		E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!

SUBROUTINE GET_UNIT(IUNIT)

  ! Find an unused unit to open a file.
  IMPLICIT NONE

  INTEGER, INTENT(OUT) :: IUNIT
  INTEGER :: i
  LOGICAL :: is_open = .FALSE.

  DO i = 200, 10000
	  INQUIRE ( UNIT=i, OPENED=is_open )
	  IF ( .NOT. is_open ) EXIT
  END DO
  IUNIT = i

END SUBROUTINE GET_UNIT
