program fibonacci
  implicit none
  integer (kind=4) :: n
  integer :: i

  print *, 'Enter an integer please.'
  print *, 'Up to 3 digits please'

  read *, n
  print '(1x,i3)', n
  
!Have user give value of n
!Have a function that accepts two args
  !user inputs n, makes two arguments for function, n-1 and n-2
  !output arguments are n and n-1

  !Fn = F(n-1) + F(n-2)
  !Fn = n
  
  !use a loop to get the series up to n
  !print out the series
  
  do i = 0,n,1
    


end program fibonacci

function func1(arg1, arg2)
  implicit none
  integer, (intent=in) :: arg1, arg2
  integer, (intent=out) :: fn, fn-1

  fn = arg1 + 1
  fn-1 = arg2 + 1

end function func1
