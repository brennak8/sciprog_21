program oddeven
  implicit none
  integer :: n, flag, x, y, z
  
  !Flag variable is used to break the do while loop
  flag = 1
  !n can be set to any positive real number
  n = 100

  do while (flag.eq.1)
    write (6, fmt = ' (i2, a)', advance='no'), f_n(n), ', '
    n = f_n(n)
    !if (n.eq.4) then
    !  x = n
    !end if
    !if (n.eq.2) then
    !  y = n
    !end if
    if (n.eq.1) then
      flag = 0
    !  z = n
    end if
    !if ((x.eq.1).and.(y.eq.2).and.(z.eq.1)) then
    !  flag = 0
    !end if
  end do

contains
  integer function f_n(n)
    implicit none
    integer, intent(in) :: n
    
    if (mod(n,2).eq.0) then
      f_n = n/2
    else
      f_n = 3*n + 1
    end if
  end function f_n
    
end program oddeven
