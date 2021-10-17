program arctanh
  implicit none
  real (kind=4), dimension(182) :: arctanh1_x, arctanh2_x
  real (kind=4) :: res1, res2, delta, x
  integer (kind=4) :: j, i, fileunit
 
  x = -0.9
  print *, 'Please provide a precision'
  print *, 'Must be a real positive number'
  read (5,*) delta


  do i = 0,181,1
    x = x + (real(i)/100.)
    arctanh1_x(i+1) = artanh1(x,delta)
    arctanh2_x(i+1) = artanh2(x)
  end do
  
  print *, arctanh1_x(4)
  print *, arctanh2_x(4)
 
contains

  real function artanh1(x,delta)
    !Uses MacLaurin series to express hyperbolic arc tangent
    real (kind=4), intent(in) :: x
    real (kind=4) :: y, y_1, delta, n
    integer :: z

    n = 0.
    y = 0.
    y_1 = 0.
    z = 1
    do while (z.eq.1)
      y = y + (x**(2.*n + 1))/(2.*n + 1)
      y_1 = (x**(2.*n + 1))/(2.*n + 1.)
      n = n + 1.
      if (abs(y_1).lt.delta) then
        z = 0
      end if
    end do
   
    artanh1 =  y

  end function artanh1

  real function artanh2(x)
    !Uses natural logarithms to express hyperbolic arc tangent
    real (kind=4), intent(in) :: x

      artanh2 = 0.5*(log((1. + x)/(1. -x)))

  end function artanh2

end program arctanh
