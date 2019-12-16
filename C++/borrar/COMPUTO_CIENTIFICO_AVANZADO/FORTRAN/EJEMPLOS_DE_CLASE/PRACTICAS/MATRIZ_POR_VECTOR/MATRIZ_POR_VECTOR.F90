program matriz_por_vector

implicit none

interface
  function mm(a,x,n) result(b)
          integer, intent(in) :: n
          real, dimension (:,:), intent(in) :: a
          real, dimension (:), intent(in) :: x
          real, dimension(size(x)) :: b 
  end function mm
end interface

real, dimension (:,:), allocatable :: mat 
real, dimension (:), allocatable :: vec

integer i,dim



dim =3
allocate(mat(dim,dim),vec(dim))

mat(1,:) = (/1,2,3/)
mat(2,:) = (/4,5,6/)
mat(3,:)= (/7,8,9/)
vec = (/10,20,30/)

print*, "MATRIZ:"
do i=1,dim
     print*,mat(i,:)
end do
print*, "VECTOR: " ,vec
print "(/)" 
print*,"MATRIZ x VECTOR: ",mm(mat,vec,dim)
print "(///)"

deallocate(mat,vec)

dim =2
allocate(mat(dim,dim),vec(dim))

mat(1,:) = (/0.1,0.2/)
mat(2,:) = (/0.3,0.4/)
vec = (/1,2,3/)

print*,"MATRIZ:"
do i=1,dim
     print*,mat(i,:)
end do

print*, "VECTOR: ",vec
print "(/)"
print*, " MATRIZ x VECTOR:  ",mm(mat,vec,dim)
print "(///)"


deallocate(mat,vec)

end program matriz_por_vector
