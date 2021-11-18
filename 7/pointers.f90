module functions 
  contains
    function allocatearray(j)
    implicit none
    integer, intent(in) :: j
    integer, pointer, dimension(:) :: pt, allocatearray

    allocate(pt(j))
    allocatearray => pt

    end function allocatearray

    function fillwithones(pt)
    implicit none
    integer :: i, k
    integer, pointer, dimension(:) :: pt, fillwithones

    k = size(pt)
    do i = 1,k,1
      pt(i) = 1
    end do
    fillwithones => pt

    end function fillwithones

    subroutine printarray(pt)
    implicit none
    integer, pointer, dimension(:) :: pt

    write(6, *) pt

    end subroutine printarray

    subroutine deallocatearray(pt)
    implicit none
    integer, pointer, dimension(:) :: pt

    deallocate(pt)

    end subroutine deallocatearray

end module functions

program main
  use functions
  implicit none
  integer :: i

  write(6, *) 'How big is the 1-D array?'
  read(5, *) i
  call printarray(fillwithones(allocatearray(i)))
  call deallocatearray(fillwithones(allocatearray(i)))

end program main
