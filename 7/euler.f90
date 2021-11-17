module subroutines
  contains
    subroutine factorial(x, poly, array)
    !subroutine to return an array filled with each separate polynomial
    implicit none
    integer, intent(in) :: x, poly
    integer :: i, array_size
    real(kind=8) :: temp1, temp2
    real(kind=8), allocatable, intent(out) :: array(:)
    
    !Initialise certain variables
    temp2 = 1.
    array_size = poly + 1
    allocate(array(array_size))
    array(1) = 1.

    !Fill array with terms for each power of x
    do i = 1,poly,1
      temp1 = real(x**i)
      temp2 = i*temp2
      array(i+1) = (temp1/temp2)
    end do

    end subroutine factorial
end module subroutines

program euler
  !Program used to approximate euler's number
  use subroutines
  implicit none
  integer :: x, usr_inp, array_size
  real(kind=8), allocatable :: array(:)

  !Initialise x to be equal to 1
  x = 1

  !User input
  write(6, *) 'Please provide the order of polynomial needed'
  read(5,*) usr_inp

  array_size = usr_inp + 1
  allocate(array(array_size))
  call factorial(x, usr_inp, array)
  print *, array

  !Sum terms to get Euler's number
  write(6, *) "Euler's number with", usr_inp, 'polynomial terms'
  write(6, *) sum(array)

  !Deallocate array, good practice
  deallocate(array)
end program euler
