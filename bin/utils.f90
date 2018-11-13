module utils

   implicit none

   public get_unit

contains

      subroutine get_unit(iunit)

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

     end subroutine get_unit

end module utils
