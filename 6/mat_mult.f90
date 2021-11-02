program matrix_mult
  use functions 
  implicit none
  integer :: i, j, k
  integer, parameter :: n = 5, p = 3, q = 4
  real (kind=8) :: A(n,p), B(p,q), C(n,q)

  !initialize c matrix
  C = 0.

  !initialize a matrix
  do i = 1,n,1
    do j = 1,p,1
      A(i,j) = i+j
    end do
  end do

  !initialize b matrix
  do i = 1,p,1
    do j = 1,q,1
      B(i,j) = i-j
    end do
  end do

  !matrix multiplication
  C = matmult(n,p,q,a,b)

  !Only print out the result of the matrix multiplication
  write(6, '(/,a,/,/)') 'This is matrix C'
  do i = 1,n
    do j = 1,q
      write(6,'(f4.0)', advance='no') C(i,j)
    end do
    write(6,*)
  end do

end program matrix_mult
