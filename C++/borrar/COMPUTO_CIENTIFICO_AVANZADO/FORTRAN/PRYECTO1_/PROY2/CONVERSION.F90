
!!modulo conversion

module conversion
use operaciones !usa el módulo operaciones

implicit none


contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************CONVERSION DE LISTA A MATRIZ DENSA********************************
!Convierte la lista que representa a la matriz densa , en la matriz densa
function conversion_a_matriz_densa(list)result(matriz_densa)
implicit none
!entrada
type(cabecera), intent(in)::list
!salida
real, dimension(:,:),pointer :: matriz_densa
!variables extras
type(lista), pointer :: lista
integer :: n, i,j
real:: val
!dimensión de la matriz rala
open(unit=1, file="n.txt",status="old", action="read")
read(1,*),n
close(1) !cierro el archivo, despuès de leer

lista=>list%head
allocate(matriz_densa(n,n)) !asigno memoria dinámicamente para la matriz densa

do i=1,n
   do j=1,n
      val=encontrar_elem(list, i, j) !busco el elemento correspondiente a A(i,j), mediante
      matriz_densa(i,j)=val           !indices, y se los asigno a matriz densa
   end do
end do


end function conversion_a_matriz_densa !fin de la función conversion_a_matriz_densa 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***************************CONVERSION DE MATRIZ DENSA A LISTA********************************
function conversion_a_lista(matriz_densa)result(lista1)
implicit none
real, dimension(:,:),intent(in):: matriz_densa
type(lista), pointer :: lista1, anterior, actual

integer :: nn,n,i,j,h=0
real :: elemento

open(unit=1, file="n.txt",status="old", action="read")
read(1,*),n
close(1) !cierro el archivo, despuès de leer  


do i=1,n
   do j=1,n !dimensión de la matriz, note que la matriz es cuadrada
      elemento=matriz_densa(i,j) 
      if (elemento/=0) then 
	  !if (matriz_densa(i,j)/=0) then
		 if (h==0) then
		    allocate(lista1)
		    lista1%valor=elemento  !asigno los valores a la lista
            lista1%indice_fila=i   !este if corresponde a la primera casilla de la lista
            lista1%indice_columna=j
			anterior=>lista1
			h=1 !luego que entra a este if, no vuelse a ejecutarse este if
		 else
		    allocate(actual)     
			anterior%sig=>actual  !asigno los valores a la lista
		    actual%valor=elemento  !esto sucede a partir de la segunda casilla de la lista
            actual%indice_fila=i
            actual%indice_columna=j
            anterior=>actual
            nullify(actual)
		 end if
	  end if
	  if (i==n .and. j==n) then !cuando se llegue a los últimos elementos se asigna
	     anterior%sig=>NULL()   !al apuntado final el valor Nulo
	     exit
	  end if
   end do      
end do
nullify(anterior)

end function conversion_a_lista

end module conversion