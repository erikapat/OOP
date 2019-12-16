
!!modulo sistemas lineales

module sistemas_lineales
use operaciones !usa el módulo operaciones
implicit none

contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************NORMA DE UN VECTOR***********************************************
!Halla la norma de un vector, muy útil para la resolución de sistemas mediante gauss-seidael
function norma(b)result(norm)
implicit none
!entrada
real, dimension(:), intent(in) :: b
!norm: salida, suma: suma de los elemnto del vector b, elevados al cuadrado
real::norm, suma=0.0
integer :: n, i !tamaño de b

n=size(b) !tamaño de b

do i=1,n-1
  if (abs(b(i+1))>abs(b(i))) then
  norm=abs(b(i+1))
 !suma=suma+b(i)*b(i) !sumo, los elemnto del vector elevados al cuadrado
  end if
end do

!norm=sqrt(suma) !y luego a la suma le hallo su raíz
!luego obtengo el valod de la norma
end function norma
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************DIAGONAL***********************************************
!Crea una lista, que contiene unicamente los elementos que se encuantran en la
!diagonal de la matriz 
function diagonal(list)result(lista3)
implicit none
!entrada
type(cabecera), intent(in)::list
!lista3: salida
type(lista), pointer :: lista, lista3, anterior, actual
integer :: h=0 !para asigna el puntero  lista a la primera casilla
!que cumpla con que sus indices sean i>=j, en la diagonal y debajo de esta


lista=>list%head !apunta a la primera casilla de la lista
do while(associated(lista)) !mientras halla un apuntador a otra casilla
   if (lista%indice_fila==lista%indice_columna) then !verificar condición, diagonal (i==j)
	   if (h==0) then                                
	      allocate(lista3)
	      lista3%valor=lista%valor !cuando se cumpla la condición asigno los valores 
          lista3%indice_fila=lista%indice_fila !a la nueva lista
          lista3%indice_columna=lista%indice_columna
          anterior=>lista3
		  h=1 !despues de asigna los valores a la primera casilla, esto asegura, a que el if
		  !no se verifique nuevamente
	   else ! a partir de la segunda casilla de la lista
	      allocate(actual)
		  anterior%sig=>actual !cuando se cumpla la condición asigno los valores
		  actual%valor=lista%valor
          actual%indice_fila=lista%indice_fila
          actual%indice_columna=lista%indice_columna
		  anterior=>actual
		  nullify(actual)
	   end if
	end if
   lista=>lista%sig !sigo a la siguiente casilla
end do
anterior%sig=>NULL()
nullify(anterior)
end function diagonal !fin de la función diagonal
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************GAUSS - SEIDEL***********************************************
!función para la resolución de sistemas lineales  mediante el método iterativo de
!gauss-seidel
function gauss_seidel(list,b,tol)result(x)
implicit none
!entrada
type(cabecera), intent(in)::list
real, dimension(:), intent(in):: b
real, intent(in):: tol
!salida
real, dimension(size(b)):: x,r,z,b_aux, mult
integer :: n,i,j,k, nmax=2000, no_nulos
type(lista), pointer::diag, d,a
type(cabecera):: list_diagonal, list_d, list_a
real:: potencia=-1, delta, norma_x, norma_r, val


n=size(b)
!inicialización
x(1:n)=0
r(1:n)=0
z(1:n)=0

diag=>diagonal(list)
list_diagonal%head=>diag

no_nulos=elem_no_nulos(list_diagonal) !llamo a la función cantidad de elemntos no nulos
if (no_nulos/=n) stop "ERROR: división entre cero"
   

!d=>exponenciacion(list_diagonal,potencia)
d=>list_diagonal**potencia
list_d%head=>d


!b_aux=mult_list_por_vector(list_d, b)
b_aux=list_d*b !multiplicación de una lista por un vector, se obtien 
!como resultado un vector
a=>list*list_d
list_a%head=>a

do k=1,nmax+1
   if (k==nmax+1) then
      print "(16x,(a))","NO HUBO CONVERGENCIA"!
      x(1:n)=0 !lo igualamos a cero para luego hacer una verificación
      exit !no se ejecutan las siguientes instrucciones
	end if
      
   do i=1,n
      x(i)=b_aux(i)
	  do j=1,n
	    if (i/=j) then
	      val=encontrar_elem(list_a, i, j) !busco los elemntos correspondientes a los indices i,j 
	      x(i)=x(i)-val*x(j)
	    end if
	  end do
   end do

   r=x-z
   norma_r=norma(r)
   z=x
   norma_x=norma(x)

   delta=norma_r/norma_x
   if (delta<=tol) then
      print "(16x,(a))","CONVERGENCIA"!
	  print "(16x,(a),\)", ""
	  exit
   end if
end do !fin del ciclo de nmax

end function gauss_seidel !fin de la función gauss_seidel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!************************** FIN MODULO ***************************************************
end module sistemas_lineales !fin modulo







