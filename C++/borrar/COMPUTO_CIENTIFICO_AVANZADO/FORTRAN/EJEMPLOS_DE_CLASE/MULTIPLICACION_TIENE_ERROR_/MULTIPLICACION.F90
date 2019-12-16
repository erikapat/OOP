! Programa principal

program multiplicacion

implicit none
type matriz_sparse
     real, dimension(:), pointer :: valor
     integer, dimension(:), pointer :: IC, ICF
     integer :: fila, columna, no_nulos
 end type matriz_sparse


type(matriz_sparse) :: matriz1, matriz2
type(matriz_sparse) :: matriz3
real :: escalar

open(unit=1, file= "data2.txt",status="old", action="read", err=99)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PRIMERA MATRIZ  !!!!!!!!!!!!!!!!!!!!!!!!!
read(1,*) matriz1%no_nulos
print *, matriz1%no_nulos                   !!!!!!!

read(1,*)matriz1%fila, matriz1%columna
print *, matriz1%fila, matriz1%columna      !!!!!!
!!!vECTORES
allocate(matriz1%valor(matriz1%no_nulos))
read(1,*) matriz1%valor(1:matriz1%no_nulos) !!!!!
print *, matriz1%valor(1:matriz1%no_nulos)  !!!!!
!!!vECTORES
allocate(matriz1%IC(matriz1%no_nulos))
read(1,*) matriz1%IC(1:matriz1%no_nulos)  !!!!
print *, matriz1%IC(1:matriz1%no_nulos)   !!!!
!!!vECTORES
allocate(matriz1%ICF(matriz1%fila+1))
read(1,*) matriz1%ICF(1:matriz1%fila+1)  !!!!
print *, matriz1%ICF(1:matriz1%fila+1)   !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SEGUNDA MATRIZ !!!!!!!!!!!!!!!!!!!

read(1,*) matriz2%no_nulos
print *, matriz2%no_nulos                   !!!!!!!

read(1,*)matriz2%fila, matriz2%columna
print *, matriz2%fila, matriz2%columna      !!!!!!

allocate(matriz2%valor(matriz2%no_nulos))
read(1,*) matriz2%valor(1:matriz2%no_nulos) !!!!!
print *, matriz2%valor(1:matriz2%no_nulos)  !!!!!

allocate(matriz2%IC(matriz2%no_nulos))
read(1,*) matriz2%IC(1:matriz2%no_nulos)  !!!!
print *, matriz2%IC(1:matriz2%no_nulos)   !!!!

allocate(matriz2%ICF(matriz2%fila+1))
read(1,*) matriz2%ICF(1:matriz2%fila+1)  !!!!
print *, matriz2%ICF(1:matriz2%fila+1)   !!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! leer vector y escalar !!!!!!!!!!!!!!!!!!

read(1,*)escalar
print *,  escalar                   !!!!!!!
print *, "____________________________________________________________"

!!!PRUEBAS

matriz3= mult_matriz(matriz1, matriz2)
print *, "____________________________________________________________"

print *, matriz3%valor  !!!!!
print *, "____________________________________________________________"
print *, matriz3%IC   !!!!
print *, "____________________________________________________________"

print *, matriz3%ICF   !!!!
print *, matriz3%no_nulos   !!!!
!!!!!!pruebas fin
!stop
99 print*, "Solo por rutina: ERROR: NO SE PUDO LEER"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

function encontrar_elem(matriz2, i, j)result(val)
implicit none
type(matriz_sparse), intent(in):: matriz2
integer, intent(in) :: i, j
real :: val
integer:: n, h, k, m 

n=size(matriz2%ICF)-1
h=1
val=0
  do k=1,n
     do m=matriz2%ICF(k), matriz2%ICF(k+1)-1
	    if (i==h .and. j==matriz2%IC(m)) val=matriz2%valor(m)
	 end do
	 h=h+1
  end do
end function encontrar_elem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function  mult_matriz(matriz1, matriz2)result(matriz3)
type(matriz_sparse), intent(in):: matriz1, matriz2
type(matriz_sparse):: matriz3

integer, dimension(:), allocatable :: a_ic, a_icf !!!!
real, dimension(:), allocatable :: a_valor !!!!
integer:: i, j, k, fil, h, suma, elem  !!!!!!
real :: val2, val1  !!!!!!

allocate(a_ic(matriz1%fila*matriz2%columna), a_valor(matriz1%fila*matriz2%columna))
allocate(a_icf(matriz1%fila+1))

h=1
fil=1
a_icf(fil)=1
elem=1
  do i=1,matriz1%fila
     do k=1,matriz2%columna
	    suma=0
        do j=1, matriz1%columna
	    val1=encontrar_elem(matriz1, i, j)
		val2=encontrar_elem(matriz2, j, k)
		suma=suma+val1*val2
		end do
	 a_valor(h)=suma
	 a_ic(h)=k
	 if (suma==0) cycle 
	 elem=elem+1
	 h=h+1
	 end do
	 fil=fil+1
	 a_icf(fil)=elem	  
  end do

allocate(matriz3%valor(h-1))
allocate(matriz3%IC(h-1))
allocate(matriz3%ICF(fil))

matriz3%fila=fil-1
matriz3%columna=fil-1

matriz3%valor(1:h-1)=a_valor(1:h-1)
matriz3%IC(1:h-1)=a_ic(1:h-1)
matriz3%ICF(1:fil)=a_icf(1:fil)
matriz3%no_nulos=matriz3%ICF(fil)-1
matriz3%no_nulos=matriz3%ICF(fil)-1

deallocate(a_ic,a_valor,a_icf)

end function mult_matriz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end program multiplicacion
