include 'magic_square.fh'
program magic_input
!Program to check if a matrix that is input
!by hand is a magic square
  use msquare
  implicit none
  integer :: n, i
  integer, allocatable :: mat(:,:)

  !Establish size of square matrix
  write(6, *) 'Please enter the size of your square matrix'
  read(5, *) n
  
  !Allocate size the matrix
  allocate(mat(n,n))
  
  !Manually input elements of each matrix row
  do i = 1,n,1
    write(6,*) 'Enter elements in row', i, 'of the matrix, separated by a space'
    read(5, *) mat(i, :)
  end do

  !Call module to check if magic square
  if (isMagicSquare(mat,n)) then
    write(6,*) 'This is a magic square'
  else
    write(6, *) 'This is not a magic square'
  end if

  !Free memory allocated to array
  deallocate(mat)

end program magic_input
