program oddeven
  implicit none
  integer :: n, flag, x, y, z, i, length, res
  character(len=40) :: line
  
  !Flag variable is used to break the do while loop
  flag = 1
  !n can be set to any positive real number
  !Change so that it takes user input maybe?
  n = 1000

  !Initialise character variable, line, and the length of line
  line = ''
  length = len(trim(line))

  do while (flag.eq.1)
    !Only need to call the function once
    !Write to the screen, using advance = 'no'
    !At the same time, we write the same output to line 
    !As we loop, length grows
    res = f_n(n)
    write (line, fmt = '(i0, a)'), res, ', '
    length = length + len(trim(line))
    write (6, fmt = '(i0, a)', advance = 'no'), res, ', '

    !Check if length of line is nearing 40
    !If it does, we print a new line and write to that
    !Initialise the variables line and length again
    if (length.le.36) then
      continue
    else if (length.ge.37) then
      print *, ''
      line = ''
      length = 0
      write (line, fmt = '(i0,a)'), res, ', '
      length = length + len(trim(line))
    end if
    n = res
    if (n.eq.4) then
      x = n
    end if
    if (n.eq.2) then
      y = n
    end if
    if (n.eq.1) then
      z = n
    end if
    !Once the last three digits are 4,2 and 1, flag changes
    !Exits do while loop
    if ((x.eq.4).and.(y.eq.2).and.(z.eq.1)) then
      print *, ''
      flag = 0
    end if
  end do

contains
  integer function f_n(n)
    !function to run the formulae supplied in the assignment
    implicit none
    integer, intent(in) :: n
    
    if (mod(n,2).eq.0) then
      f_n = n/2
    else
      f_n = 3*n + 1
    end if
  end function f_n
    
end program oddeven
