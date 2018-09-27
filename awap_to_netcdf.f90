!--------------------------------------------------------------------------!
!		     		M A I N   P R O G R A M								               !
!--------------------------------------------------------------------------!
PROGRAM awap_to_netcdf

	use utils, only: get_unit
	implicit none

	integer :: iunit

	! 1. Initialise variable, arrays to store things, etc
	! 2. Loop over years
	! Read header file
	! Open AWAP file for each variable and fill up arrays.
	! loop over years, days
	! open AWAP file
	! call weather generator
	! write netcdf file




	call get_unit(iunit)

	print *, iunit


END PROGRAM awap_to_netcdf

!--------------------------------------------------------------------------!
!		     		E N D   P R O G R A M								               !
!--------------------------------------------------------------------------!
