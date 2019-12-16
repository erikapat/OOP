program test

use tipos

implicit none

type(longitud):: s=longitud(10.0),l
type(longitud_al_cuadrado):: s2=longitud_al_cuadrado(10.0) 
type(velocidad) ::v
type(tiempo):: t=tiempo(3.0)

v=s/t

!NOTA: v=s+t; v=s*t SONILEGALES

t=t+tiempo(1.0) 
l=sqrt(s2)
print*, "v",v," t= " ,t ," l=", l


end program test

