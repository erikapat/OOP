module tipos

implicit none

type tiempo
real segundos
end type tiempo

type velocidad
real metros_por_segundo
end type velocidad

type longitud
real metros
end type longitud

type longitud_al_cuadrado
real metros_cuadrados
end type longitud_al_cuadrado

interface operator(/)
module procedure longitud_entre_tiempo 
end interface


interface operator(+)
module procedure tiempo_mas_tiempo
end interface

interface sqrt
module procedure raiz_metros_cuadrados
end interface


contains


function longitud_entre_tiempo(s,t)
     type(longitud), intent(in):: s
     type(tiempo), intent(in):: t
     type(velocidad) :: longitud_entre_tiempo

      longitud_entre_tiempo%metros_por_segundo= s%metros / t%segundos 
end function longitud_entre_tiempo

function tiempo_mas_tiempo(t1 ,t2)
       type(tiempo), intent(in) ::t1, t2
       type(tiempo) ::tiempo_mas_tiempo

       tiempo_mas_tiempo%segundos = t1 %segundos + t2%segundos
 end function tiempo_mas_tiempo

function raiz_metros_cuadrados(s)
type(longitud_al_cuadrado), intent(in):: s
type(longitud) raiz_metros_cuadrados

raiz_metros_cuadrados%metros = sqrt(s%metros_cuadrados) 
end function raiz_metros_cuadrados


end module tipos

