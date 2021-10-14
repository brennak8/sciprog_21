program arctanh
  implicit none
  real (kind=8), dimension(181) :: arctanh1_x, arctanh2_x
  real (kind=8) :: res1, res2, delta, x
  integer (kind=4) :: j, i
 
  x = -0.9
  print *, 'Please provide a precision'
  print *, 'Must be a real positive number'
  read (5,*) delta
  
!  do i = 1,182,1
!    x = x + (real(i)/100.)
!    res1 = artanh1(x,delta)
!    print *, res1
!    res2 = artanh2(x)
!    print *, res2
!    arctanh1_x(i) = res1
!    arctanh2_x(i) = res2
!  end do

  print *, artanh1(x,delta)
  x = x + 0.01
  print *, artanh1(x,delta)
  
 
contains
  real function artanh1(x, delta)
    !Uses MacLaurin series to express hyperbolic arc tangent
    real (kind=8), intent(in) :: x, delta
    real (kind=8) :: y
    real (kind=8) :: n = 0.
    integer (kind=4) :: z

    z = 1

    do while (z.eq.1)
      y = y + (x**(2.*n + 1.))/(2.*n + 1.)
      print *, y, 'y', x
      n = n + 1.
      print *, n, 'n', x
      if (y.lt.delta) then
        z = 0
      end if
    end do

    artanh1 = y

  end function artanh1

  real function artanh2(x)
    !Uses natural logarithms to express hyperbolic arc tangent
    real (kind=8), intent(in) :: x

      artanh2 = 0.5*(log(1. + x) - log(1. - x))

  end function artanh2

end program arctanh
