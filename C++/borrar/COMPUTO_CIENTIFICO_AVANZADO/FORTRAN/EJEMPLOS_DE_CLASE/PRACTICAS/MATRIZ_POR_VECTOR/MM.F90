function mm(a,x,n) result(b)

implicit none
integer, intent(in) :: n
real, dimension (:,:), intent(in) :: a
real, dimension (:), intent(in) :: x
real, dimension(size(x)) :: b
integer i,j

do i=1,n
b(i)=0
do j= 1 ,n
b(i) = b(i) + a(i,j)*x(j)
enddo
enddo

end function mm
