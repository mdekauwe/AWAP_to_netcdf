!--------------------------------------------------------------------------!
!		     		M A I N   P R O G R A M								               !
!--------------------------------------------------------------------------!
PROGRAM awap_to_netcdf

	implicit none

	integer :: iunit

	! 1. Iniitalise variable, arrays to store things, etc
	! 2. Loop over years
	! Read header file
	! Open AWAP file for each variable and fill up arrays.
	! loop over years, days
	! open AWAP file
	! call weather generator
	! write netcdf file




	CALL get_unit(iunit)

	print *, iunit


END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     		E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!

SUBROUTINE get_unit(iunit)

  ! Find an unused unit to open a file.
  implicit none

  integer, intent(out) :: iunit
  integer :: i
  logical :: is_open = .FALSE.

  do i = 200, 10000
	  inquire ( UNIT=i, OPENED=is_open )
	  if ( .NOT. is_open ) exit
  end do
  iunit = i

END SUBROUTINE get_unit
