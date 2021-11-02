module functions
  contains
    function matmult(n,p,q,a,b) result(c)
      implicit none
      integer :: i, j, k
      integer, intent(in) :: n,p,q
      real (kind=8), intent(in) :: a(n,p), b(p,q)
      real (kind=8) :: c(n,q)
 
      do i = 1,n,1
        do j = 1,q,1
          do k = 1,p,1
            c(i,j) = c(i,j) +  a(i,k)*(b(k,j))
          end do
        end do
      end do

      return
    end function matmult
end module functions
