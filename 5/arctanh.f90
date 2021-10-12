program arctanh
 ! use module functions
  implicit none
  real (kind=8) :: x, delta, n, arctanh_x

  

end program arctanh

module functions
  interface
    real function artanh(n)
      !Uses MacLaurin series to express hyperbolic arc tangent
      implicit none
      real (kind=8), intent(in) :: n
      real (kind=8) :: x

      artanh1 = (x**(2*n + 1))/(2*n + 1)
    end function artanh1

    real function artanh2(n)
    !Uses natural logarithms to express hyperbolic arc tangent
    implicit none
    real (kind=8), intent(in) :: n
    real (kind=8) :: x

    artanh2 = 0.5(log(1. + x) - log(1. - x))
    end function artanh2
  end interface
end module functions
  
