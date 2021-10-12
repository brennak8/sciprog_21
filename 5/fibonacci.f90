program fibonacci
  implicit none
  integer :: Fn, Fn_1, Fn_2
  integer :: F0, F1, i, n, ierr = 0
  
  F0 = 0
  F1 = 1


  write (6,*) 'Pick an integer please'
  write (6,*) 'Please no more than 48, after that we get negative numbers'
  if (ierr.eq.0) then
    read(5,*, iostat = ierr) n
  else
    write (6,*) 'Problem with input'
  end if 

  call my_sub(F1, F0, Fn, Fn_1)
  print *, F0
  print *, F1
  print *, Fn

  !loop starts at 3 due to the fact that the subroutine was called before
  do i = 3,n,1
    Fn_2 = Fn_1
    Fn_1 = Fn
    call my_sub(Fn_1, Fn_2, Fn, Fn_1)
    print *, Fn
  end do

end program fibonacci

subroutine my_sub(arg1, arg2, res1, res2)
  implicit none
  integer, intent(in) :: arg1, arg2
  integer, intent(out) :: res1, res2
  !arg1 = Fn_1, arg2 = Fn_2, res1 = Fn, res2 = Fn_1
  
  res1 = arg1 + arg2
  res2 = arg1

end subroutine my_sub
