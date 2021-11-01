program geometric_series
  implicit none
  real (kind=4) :: a, r
  integer (kind=4) :: n, i
  real (kind=4), dimension(3) :: x, y

  !Loop to run through the different cases of the switch/case construct
  do i = 1,3
    select case (i)
    !Stores S_n_form in x array
    !and S_n_sum in y array
      case (1)
        n = 10000
        a = 2.0
        r = 0.01
        x(i) = S_n_form(n,a,r)
        y(i) = S_n_sum(n,a,r)
      case (2)
        n = 500
        a = 0.01
        r = 1.1
        x(i) = S_n_form(n,a,r)
        y(i) = S_n_sum(n,a,r)
      case (3)
        n = 100
        a = 0.0001
        r = 2.0
        x(i) = S_n_form(n,a,r)
        y(i) = S_n_sum(n,a,r)
    end select
    print *, 'Using the formula'
    print *, x(i)
    print *, 'Using summation'
    print *, y(i)
    print *, ''
  end do

!-----------------------------!
!                             !
!   Comments on the Results   !
!                             !
!-----------------------------!

!Multiple executions see the method of the formula provide consistent numbers.
!However, the numbers provided by the summation method can vary over multiple
!executions.
!It seems that using the method of summation allows for more room for error.
!This can be attributed to more floating point operations during the loop and 
!the rounding errors associated with this.

contains 
  real function S_n_form(n,a,r)
    !Function to find Sn using the formula provided
    implicit none
    integer (kind=4), intent(in) :: n
    real (kind=4), intent(in) :: a, r

    S_n_form = (a*(1. - r**(real(n) + 1.)))/(1. - r)

  end function S_n_form

  real function S_n_sum(n,a,r)
    !Function to find Sn by the method of summation
    implicit none
    integer (kind=4), intent(in) :: n
    real (kind=4), intent(in) :: a, r
    integer (kind=4) :: i

    do i = 0,n,1
      S_n_sum = S_n_sum + a*(r**i)
      if (S_n_sum.lt.0.) then
        print *, 'Something has gone wrong'
        exit
      end if
    end do

  end function S_n_sum

end program geometric_series
