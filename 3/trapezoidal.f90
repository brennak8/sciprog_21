program trapezoidal_rule
  implicit none
  integer (kind = 4) :: i
  real (kind = 8) :: y, area
  real (kind = 8), parameter :: a = 0., b = 3.1415927/3.
  real (kind = 8), parameter :: n = 12., log_2 = log(2.)

  !start loop from 0 to n
  do i = 0,int(n),1
  !First and last terms in expression not multiplied by two
    if ((i.eq.0) .or. (i.eq.n)) then
      y = y + (tan(b/n * i))
    else
  !Inner terms of loop are multiplied by two
      y = y + 2*(tan(b/n * i))
    end if
  end do


  !Refers to the area under the curve
  area = ((b - a)/(2.*n) * y)

  !Difference between calculated area and log(2)
  print *, 'Area under curve: ',area
  print *, 'log(2): ', log_2
  print *, 'Difference: ', (area - log_2)

end program trapezoidal_rule
