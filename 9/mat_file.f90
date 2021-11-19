include 'magic_square.fh'
program magic_main
!Program to determine if a matrix read from a file
!is a magic square
  use msquare
  implicit none
  integer :: nlines, ierr, i
  integer, allocatable :: array(:,:)
  character(len = 75) :: filename

  nlines = 0
  !Obtain name of file from user
  write(6, *) "What's the name of your matrix file?"
  read(5, *) filename

  !Open a file defined by the user
  open(unit=11,file=filename,form='formatted',access='sequential', &
       action='read',status='old',iostat=ierr)

  !Get number of lines in the file
  do
    read(11,*,iostat=ierr)
    !Break out of loop if we can't read a line
    if (ierr.ne.0) then
      exit
    end if
    nlines = nlines + 1
  end do
  
  !Go back to the top of the file
  rewind(unit=11)

  !Allocate square matrix
  allocate(array(nlines,nlines))

  !Read each line to the array
  do i = 1,nlines
    read(11, *, iostat=ierr) array(i, :)
  end do
  
  !Print out square matrix
  do i = 1,nlines
    write(6, *) array(i,:)
  end do

  !Call the module in the header file
  if (isMagicSquare(array, nlines)) then
    write(6,*) "It's a magic square!"
  else
    write(6,*) "It's not a magic square :("
  end if

  !Deallocate array and close file
  deallocate(array)
  close(unit=11, status='keep')

end program magic_main
