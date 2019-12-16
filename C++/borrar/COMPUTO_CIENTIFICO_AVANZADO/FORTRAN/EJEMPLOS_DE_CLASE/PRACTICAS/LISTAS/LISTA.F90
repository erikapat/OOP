!EJEMPLO DE LISTAS
program listas

implicit none

type item
  real ::val
  type(item), pointer ::sig
end type item
	
integer::	ios,cont=0
type(item) , pointer ::actual, primero, anterior

open(unit=1,file="lst.txt" ,err=99)

allocate(primero)
read(1,fmt= "(F6.2)" ,advance="no",iostat = ios)primero%val

if (ios /= 0) then
   deallocate(primero) ; 
   close(1)
   stop "No hay datos que procesar (archivo vacio)"
end if

!Crea la lista
	ios = 0	
allocate(actual)
primero%sig => actual 

do while (ios == 0)
    read(unit=1,fmt=" (F6.2) ",advance='no',iostat = ios)actual%val
    if (ios /= 0) exit
    cont=cont+1	
    anterior => actual 	
    allocate (actual)	
    anterior%sig => actual	
enddo

!Recorre la lista para imprimir
	actual => primero	
do
	write(*,"(F8.2)",advance="no")actual%val !---------->imprime en pantala debido al (*)
	print "(//)"	
	if(.not. (associated(actual%sig))) exit	
	actual => actual%sig	
enddo
	close (1)	
	deallocate (actual ,primero) 
stop

99 print*, "Se genero un error tratando de abrir el archivo" 
end program listas
