module functions
  contains
    integer function iter(a,b)
    !Function to use an euclidian algorithm in an iterative manner
      integer(kind=4), intent(in) :: a, b
      integer(kind=4) :: x, y, temp
      x = a
      y = b
      if ((x.eq.0).or.(y.eq.0)) then
        write(6, *), 'Arguments can not be zero'
      end if
      do while (y.ne.0)
        temp = y
        y = mod(x,y)
        x = temp
      end do
      iter = x

    end function iter

    integer recursive function recur(a,b) result(answer)
    !Function to use an euclidian algorithm in a recursive manner
      integer(kind=4), intent(in) :: a, b
      integer(kind=4) :: x, y, temp
      x = a
      y = b
      if ((x.eq.0).or.(y.eq.0)) then
        write(6, *), 'Arguments can not be zero'
      end if
      temp = y
      y = mod(x,y)
      x = temp
      answer = x
      if (y.ne.0) answer = recur(x,y)
      return

    end function recur
end module functions

program euclid
  use functions
  implicit none
  integer(kind=4) :: a, b

  write(6, *), 'Please supply a positive integer'
  read(5, *), a
  write(6, *), 'Please supply another positive integer'
  read(5, *), b

  write(6, *), 'Will calculate GCD of ', a, 'and', b

  if ((a.le.0).or.(b.lt.0)) then
    write(6, *), 'Must be greater than zero'
    write(6, *), 'Incorrect input'
  else
    write(6, *), iter(a,b), 'via iterative algorithm'
    write(6, *), recur(a,b), 'via recursive algorithm'
  end if

end program euclid
