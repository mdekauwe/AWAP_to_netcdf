MODULE bios_io_mod !    MMY

USE bios_type_def

    CONTAINS
    
! From ./core/biogeophys/cable_common.F90

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

 
 
 
 
 
! **************************** MMY ***************************
SUBROUTINE read_filename(met_path,file_path,file_name) 
   implicit none
   
   INTEGER :: iunit
   CHARACTER(200) :: met_path,file_path
   CHARACTER(200), POINTER :: file_name ？？？？？？？？ 字符串可以用指针吗 在大型机上试一下
   !, DIMENSION(:) 
  
   ! read file name
   ok = system('cd '//met_path//file_path)
   ok = system('find -name "*.flt" | sort -n > namelist.txt')
  
   CALL GET_UNIT(iunit)
   OPEN (iunit, file="namelist.txt",status="old",action="read")
   READ (iunit, *)file_name
   CLOSE(iunit)
    
   ok = system('rm namelist.txt')

END SUBROUTINE get_unit(iunit)
 

 
 
 
    
! From MODULE bios_misc_io./offline/cable_bios_met_obs_params.F90

! CALL ReadArcFltHeader(iunit,landmaskhdr_file(may be also okay with other .hdr file),MaskCols,MaskRows,MaskBndW, &
!											MaskBndS,MaskRes,NoDataVal) in cable_bios_init
  SUBROUTINE ReadArcFltHeader(iunit,Filename,Cols,Rows,xLL,yLL,CellSize,NoDataVal)
!-------------------------------------------------------------------------------
! Read the header file (.hdr) associated with an ArcGIS binary grid file (.hdr).
! The elements are req'd in standard order, although ArcGIS is not reliant on this
! order. A seventh line is often found specifying the endianness, but this is not
! read. Return the grid dimensions and no data value.
!-------------------------------------------------------------------------------
  implicit none
  integer(i4b)    ,intent(in) :: iunit
  character(200)  ,intent(in) :: Filename
  integer(i4b)    ,intent(out):: Cols, Rows
  real(sp)        ,intent(out):: xLL, yLL, CellSize, NoDataVal
  character(12)               :: Head
!-------------------------------------------------------------------------------
  open (unit=iunit,file=Filename,status='old')

  read (iunit,*) Head, Cols      ! Number of columns
  read (iunit,*) Head, Rows      ! Number of rows
  read (iunit,*) Head, xLL       ! Western boundary
  read (iunit,*) Head, yLL       ! Southern boundary
  read (iunit,*) Head, CellSize  ! Resolution (both W-E, N-S)
  read (iunit,*) Head, NoDataVal ! Missing data value
  close (unit=iunit)

  END SUBROUTINE ReadArcFltHeader

END MODULE bios_io_mod  ! MMY