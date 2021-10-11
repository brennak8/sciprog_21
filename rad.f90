program rad
  implicit none
  integer :: i
  real (kind=4) :: radang
  real (kind=4) :: x_deg(13), x_rad(13), x_tan(13)
  real (kind=4) :: trap, area

  !Have (i+1) as the array index as fortran arrays start at 1, not 0
  !Generates values between 0 and 60 which are converted into radians
  !Radians are stored in x_rad array
  !Tan(x_rad) is obtained and stored in x_tan
  do i = 0,12,1
    x_deg(i+1) = i*5.
    call degtorad(x_deg(i+1), radang)
    x_rad(i+1) = radang
    x_tan(i+1) = tan(x_rad(i+1))
  end do

  write(*,*) 'Array containing degrees converted into radians'
  write(*,*) x_tan

  area = trap(12.)
  write(*,*) 'The area under the curve is ', area

end program rad

subroutine degtorad(arg,arg2)
!Subroutine coverts degrees to radians
  implicit none
  real (kind=4), intent(in) :: arg
  real (kind=4), intent(out) :: arg2
  real (kind=4) :: pi = 3.1415927

  arg2 = (pi*arg)/180.0
end subroutine degtorad

real function trap(n)
!Function uses the trapezoidal rule to calculate area under a curve
  implicit none
  real (kind=4), intent(in) :: n
  real (kind=4) :: radang
  integer :: i
  real (kind=4) :: y
  real (kind=4) :: b = 60.

  do i = 0,int(n),1
  !First and last terms in expression not multiplied by two
    call degtorad ((b/n) * i, radang)
    if ((i.eq.0) .or. (i.eq.n)) then
      y = y + tan(radang)
      print*, y
    else
  !Inner terms of loop are multiplied by two
      y = y + 2*(tan(radang))
    end if
  end do
 
  !call degtorad(b, radang)

  !Gives the absolute value of the area calculated
  trap = (radang - 0.)/(2.*n) * y
end function trap
