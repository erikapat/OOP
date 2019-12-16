
program carros 

use operaciones_con_carros

implicit none

type(carro):: toyota, ford


toyota= carro(25000.0,1500.0,3) 
ford = carro(17000.0,1500.0,2)

print*,"TOYOTA ",toyota	
print*,"FORD ",ford
print "(/)"


if (toyota == ford) then
   print*, "Son iguales"
else
   print*, "Son diferentes"
end if

print "(///)"
print*,  "Despues de asignacion :" 
print "(/)"

toyota = ford

print*,"TOYOTA: ",toyota 
print*,"FORD ",ford 
print "(/)"


if (toyota == ford) then
   print*, "Son iguales"
else
   print*," Son diferentes"
endif

end program carros


