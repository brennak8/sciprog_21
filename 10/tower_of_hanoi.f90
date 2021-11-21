module functions
  contains
    integer recursive function move_recur(n,rod_1, rod_3, rod_2) result(num)
    !Recursive function to move disks from each rod
    implicit none
    integer, intent(in) :: n
    character(len=1), intent(in) :: rod_1, rod_2, rod_3

    !Need to fix how this works
    !Want a counter for the number of times the fucntion is called
    !Can I save a variable between function calls?
    num = num + 1
    print *, num

    !Checks if we are moving just one disk
    !i.e. we just move disk 1 from rod 1 to rod 3
    !Then leave function
    if (n.eq.1) then
      write(6,*) 'Move disk ', n, ' from rod ', rod_1, ' to rod ', rod_3
      return
    end if

    !Keeps calling the function until we get to n = 1
    print*, move_recur((n-1), rod_1, rod_2, rod_3)
    write(6,*) 'Move disk ' , n,' from rod ', rod_1, ' to rod ', rod_3
    print *, move_recur((n-1), rod_2, rod_3, rod_1)

    end function move_recur

end module functions

program tower_of_hanoi
  use functions
  implicit none
  integer :: n

  !User input, how many disks
  write(6,*) 'How many disks to move?'
  read(5,*) n

  !A, B and C are the rod names
  write(6,*) move_recur(n,'A', 'C', 'B')

end program tower_of_hanoi
