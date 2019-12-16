program test_opvec 
use OperacionesVectoriales

implicit none
type(vector) :: v1 =vector( 1.0,2.0,3.0) ,v2=vector(4.0,5.0,6.0), v3 
real(kind=8) :: escalar

print*,"Los vectores son:"
print*,""
print*,"v1=",v1, " v2=",v2
escalar= 5.0 
v3 = escalar*v1 
print*, ""

print*, "5*v1=" , v3

v3 = v1 * v2

print*, ""
print*, "v1.*v2=" , v3
print*, ""

print*, ""
print*, " Finalmente, <v1 ,v2> = ",v1 .producto_interno.v2 
print*, ""
end program test_opvec
