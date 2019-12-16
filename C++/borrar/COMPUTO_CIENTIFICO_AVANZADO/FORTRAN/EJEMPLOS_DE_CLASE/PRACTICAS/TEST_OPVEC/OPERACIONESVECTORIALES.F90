module OperacionesVectoriales

implicit none

public operator(*), operator(.producto_interno.)
private:: producto_escvec, producto_vecvec, producto_interno

type vector
     real(kind=8) x,y,z
end type vector

public vector

interface operator(*)
   module procedure producto_escvec
   module procedure producto_vecvec
end interface

interface operator(.producto_interno.)
   module procedure producto_interno
end interface


contains


function producto_escvec(a,v) result(w)
   real(kind=8), intent(in) ::a
   type(vector), intent(in) ::v
   type(vector) w

   w%x=a*v%x; w%y=a*v%y ; w%z=a*v%z 
end function producto_escvec

function producto_vecvec(v1 ,v2) result(w)
   type(vector), intent(in) :: v1 ,v2
   type(vector) w

   w%x = v1%x * v2%x ; w%y = v1%y * v2%y ; w%z = v1%z * v2%z 
end function producto_vecvec

function producto_interno(v1,v2) result(w)
  type(vector), intent(in) ::v1 ,v2
  real(kind=8) :: w

  w=v1%x*v2%x +v1%y*v2%y +v1%z*v2%z
end function producto_interno

end module OperacionesVectoriales
