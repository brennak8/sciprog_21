program test
  implicit none
  integer :: n, num, num_1, num_2 
  integer :: i, ierr = 0

  write (6,*) 'Type an integer please'
  write (6,*) 'Please no more than three digits'
  if (ierr.eq.0) then
    read(5,*, iostat = ierr) n
  else
    write (6,*) 'Problem with input'
  end if
  print*, n

  num_1 = n-1
  num_2 = n-2
  

  do i = 0, n, 1
    if ((i.eq.0).or.(i.eq.1)) then
      print *, i, ' = ', i 
    else
      print *, i, ' = ', i-1, ' + ', i-2
     !Only print numbers that satisfy this sequence
    
    !if number satisfies expression, then
    !print number and expression
    !use subroutine to loop through different numbers
    !Look up online
    !number in sequence is made of the numbers in sequence before it
!      call my_sub(num_1, num_2, num, num_1)
    end if
  end do

  !Make a function that takes num_1, num_2
  !Function should give back num, num_1
  !Use a subroutine for now
    
end program test

subroutine my_sub(arg1, arg2, res1, res2)
  implicit none
  integer, intent(in) :: arg1, arg2
  integer, intent(out) :: res1, res2

  res1 = arg1 + 1
  res2 = arg2 + 1

end subroutine my_sub
