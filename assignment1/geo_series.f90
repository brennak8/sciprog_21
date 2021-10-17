program geometric_series
  implicit none
  real (kind=4) :: a, r, x
  integer (kind=4) :: n, i

  i = 1
  print *, S_n(10000, 2.0, 0.01)

  print *, S_n(500, 0.01, 1.1)

  print *, S_n(100, 0.0001, 2.0)

  !do i = 1,3
  !  print *, x
  !end do

  select case (i)
    case (1)
      x = S_n(10000, 2.0, 0.01)
      n = 10000
      a = 2.0
      r = 0.01
    case (2)
      x = S_n(500, 0.01, 1.1)
      n = 500
      a = 0.01
      r = 1.1
    case (3)
      x = S_n(100, 0.0001, 2.0)
      n = 100
      a = 0.0001
      r = 2.0
    case default
      print *, 'Invalid'
  end select 

  do i = 1,3
    print *, i
    !print *, x
    print *, S_n(n,a,r)
  end do

contains 
  real function S_n(n,a,r)
    implicit none
    integer (kind=4), intent(in) :: n
    real (kind=4), intent(in) :: a, r

    print *, n, 'n'
    print *, a, 'a'
    print *, r, 'r'
    S_n = (a*(1. - r**(real(n) + 1.)))/(1. - r)

  end function S_n
end program geometric_series
