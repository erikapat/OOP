
                                !MÓDULO OPERACIONES
!Módulo que representa las matrices esparcidas, y permite operaciones algebraicas entre ellas
!(suma, resta, multiplicación) junto con la comparación y lectura de los elemntos de 
!dicha matriz

module operaciones

implicit none
!DECLARO LAS OPERACIONES (recarga de operadores)
!funciones a las cuales sólo accedera el modulo (son las funciones que definen
!las operaciones bàsicas con listas)
private :: lectura, suma_lista, resta_lista, mult_escalar, mult_lista, comparacion, &
exponenciacion, mult_list_por_vector
!DEFINO LA ESTRUCTURA DE LA LISTA, EN LA CUAL SE ALMACENARAN LOS ELEMENTOS
!NO NULOS DE LA MATRIZ, JUNTO CON LOS INDICES DE FILA Y COLUMNA DE LA MISMA
type lista
  real:: valor
  integer:: indice_fila, indice_columna
  type(lista), pointer::sig
end type lista
!PUNTERO AL PRIMER ELEMENTO DE LA LISTA (se usará como parametros en las funciones y
!subrutinas, para acceder a las listas con los datos)
type cabecera 
  type(lista), pointer ::head
end type cabecera

!interfaces
interface operator(.lectura.) !OPERACION DE LECTURA
 module procedure lectura 
end interface

interface operator(+)
 module procedure suma_lista !suma dos matrices de la misma dimensión
end interface

interface operator(-)
 module procedure resta_lista !resta dos matrices de la misma dimensión
end interface

interface operator(*)
 module procedure mult_list_por_vector !se multiplica una lista por un vector
 !y el resultado es un vector, este operador se utilizara en la función gauss 
 !seidel, en el modulo sistemas_lineales
 module procedure mult_escalar !multiplicación por un escalar
 module procedure mult_lista     !multiplicaciòn de matrices a(i,j)*b(j,k), donde 
end interface                     ! k e i puden tener valores enteros diferentes           
                                  !            a(i,j)*b(j,k) = c(i,k)

interface operator(==)
 module procedure comparacion !verifica que dos matrices sea iguales o no,
end interface

interface operator(**)
 module procedure exponenciacion !elevar un elemento de la lista a un número real
end interface                    !se usará en el modulo de operacines lineales, para
                                 !resolver sistemas lineales mediante gauss-seidel

contains




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!***********************ENCONTRAR ELEMENTO**********************************************
!Dada la lista y su índice columna y fila, se busca en la lista el valor del elemento no
!nulo, si existe, en caso contrario, (es decir, en caso de no encontrar el elemento), se 
!coloca cero
function encontrar_elem(list, i, j)result(val)
implicit none
!entrada
type(cabecera), intent(in)::list !puntero al inicio de la lista
integer, intent(in) :: i, j !indices
type(lista), pointer :: lista, actual !variables auxiliares
!salida
real :: val !elemento ubicado en A(i,j), siendo A la matriz rala 

lista=>list%head !usamos la variable auxiliar lista y actual, para recorrerla lista
actual=>lista
val=0 !devuelve cero en caso de no encontrar nada
do while(associated(actual))
   if (i==actual%indice_fila.and.j==actual%indice_columna) then !si i,j coinciden con
!algún indice en la lista, hemos encontrado el elemento no nulo buscado
   val=actual%valor !así que reviso en esa fila
   exit !cuando encontramos el valor salimos del ciclo
   end if
   actual=>actual%sig !pasamos al siguiente elemnto de la lista
end do
nullify(actual, lista) !liberamos memoria

end function encontrar_elem !fin de función

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!************************* ELEMENTOS NO NULOS ********************************************
!Esta función cuenta el número de elementos no nulos que contiene la fila
function elem_no_nulos(list)result(no_nulos)
implicit none
!entrada
type(cabecera), intent(in)::list 
!punteros a lista
type(lista), pointer :: lista, actual
!salida
integer:: no_nulos
!inicialización
no_nulos=0 
lista=>list%head !apuntamos al primer elemnto de la lista
actual=>lista
do while(associated(actual))
   no_nulos=no_nulos+1 !se cuentan la cantidad de casillas con datos que tiene la lista
   actual=>actual%sig !pasamos a la siguiente casilla de la lista
end do
nullify(actual, lista)!liberamos memoria
end function elem_no_nulos !fin de función
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!********************************* LECTURA **********************************************
!Se leen los datos del archivo ingresado por el usuario, con el objetivo de guardalos en 
!la lista, con la cual luego se realizarán operaciones diversas
function lectura(arch_data)result(lista1)
implicit none
!salida
type(lista), pointer:: lista1
type(cabecera):: list
!entrada
character(len=*), intent(in):: arch_data
!punteros a lista
type(lista), pointer::actual, anterior
integer::i, no_nulos, n, ios
real, dimension(:), allocatable :: b !vector b
!archivos a utilizar, el primero es un archivo de lectura, que contiene los datos de la matriz
!rala
open(unit=1, file=arch_data,status="old", action="read", iostat=ios)
call error(ios) !indica los errores frecuentes que pueden suceder al abrir o leer un archivo
!el segundo archivo, es de escritura, nos servirá para guardar la dimensión de la matriz
!y poder acceder fácilmente a ella cuando sea necesario 
open(unit=2, file="n.txt",status="unknown", action="write",iostat=ios)
call error(ios)
open(unit=3, file="b.txt",status="unknown", action="write",iostat=ios)!abro archivo llamado
!b.txt para guardar los valores del vector b y usarlos cuando sea necesario
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error

!!!!
!LISTA 
read(1,*,iostat=ios) n !----->guardamos esta variable en un archivo, al cual se pueda acceder cada vez que
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error

!sea necesario
write(2,*)n !----->guardo esta variable en un archivo con su mismo nombre, para acceder
!fácilmente a ella en cualquier momento
read(1,*, iostat=ios) no_nulos !-----> cantidad de elemntos no nulos que contiene la matriz
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
!verifica que n y no_nulos cumpla las condiciones necesarias para la ejecución del programa
!como n y no_nulos positivos y diferentes de cero y no_nulos no mayor al cuadrado de n
call ver_valores_n_no_nulos(n,no_nulos)
! LEE una LISTA  

allocate(lista1)!asigna memoria a el puntero lista1, que es la primera casilla de la lista
read(1,*, iostat=ios)lista1%valor, lista1%indice_fila, lista1%indice_columna !lee tres valore
!elemento no nulo (valor), índice de columna e índice de fila
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error

anterior=>lista1 !apuntamos anterior al primer elemento de la lista, con la finalidad
!de crear la lista mientras nos movemos en ella

do i=1,no_nulos-1 !leimos el primer elemento así que restan por leer la cantidad de
!elementos no nulos menos 1
   allocate(actual) !asignamos memoria
   if (i<=no_nulos) then
      anterior%sig=>actual !nos movemos en la lista gracias a la variable puntero a lista
	  !llamada anterior
   else
      anterior%sig=>NULL() !si ya no hay màs elemntos que leer entonces el último elemento
	  !no debe tener puntero, así que los anulamos
	  exit !y salimos
   end if !si todavia hay elementos que leer, los leemos de la misma forma 
   !que leimos el primer elemento
   read(1,*, iostat=ios)actual%valor, actual%indice_fila, actual%indice_columna
   !ahora anterior apunta a la casilla actual, donde acabamos de ingresar los datos
   call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
   anterior=>actual
   nullify(actual) !borramos todo contenido de actual para después asignarle otros valores
end do !fin del ciclo, ya no hay elementos no nulos
nullify(anterior) !libero memoria de las variables auxiliares

allocate(b(n))
read(1,*,iostat=ios) b(1:n)
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
write(3,*)b(1:n) !guardo en archivo en vector b
close(1)!cierro el archivo de lectura
close(2)!cierro el archivo de escritura, donde guarde el valor de n (dimensión)
close(3)!cierro archivo en el cual guarde el valor del vector b
list%head=>lista1
call verificar_dat_entrada(list) !

contains
!************** DENTRO DE ESTA FUNCIÓN TENERMOS 2 SUBPROGRAMAS **********************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBPROGRAMA ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN:! función error: si el formato del archivo es diferente al especificado, 
!se produce un error y si no exixte el archivo especificado, o este esta mal escrito
!también arroja un error
!cualquiera de los dos errores detiene la ejecución programa
!1.- NOMBRE Y FORMATO DE ARCHIVO
 subroutine error(ios)
   implicit none
   integer, intent(in):: ios
   if (ios==29) then !el número 29 indica un error de falla de lectura
   print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
	   print "(11x,(a))", "                   FALLA DE LECTURA                       " 
       print "(11x,(a))","(        Nombre incorrecto o archivo Inexistente           )"
	   print "(//)"
       stop
   else if (ios/=0 .and. ios/=29) then
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
	   print "(11x,(a))", "                   FALLA DE LECTURA                       " 
       print "(11x,(a))","(Verifique que su archivo cumpla con el formato establecido)"
	   print "(//)"
       stop
   end if
 end subroutine error
 !*********************************************************************************************
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!SUBPROGRAMA VER_VALORES_N_NO_NULOS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !vERIFICA QUE SE CUMPLA LAS CONDICIONES:
!1.- n y no_nulos deben ser mayores o iguales a 1, (debe quedar claro que usar este tipo de 
!programa para una lista de 1 elemento resulta ineficiente), no_nulos no debe sobrepasar el valor
!de nxn  
 subroutine ver_valores_n_no_nulos(n,no_nulos)
 implicit none
 integer, intent(in):: n,no_nulos
    if (n<=0 .or. no_nulos<=0) then 
   print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
       print "(11x,(a))","( el valor de n y no_nulos debe ser mayor estricto a cero  )"
	   print "(//)"
       stop
   else if (no_nulos>n*n) then
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
       print "(10x,(a))", "NO PUEDE HABER MAS ELEMENTOS NO_NULOS QUE ELEMENTOS DE LA MATRIZ   " 
       print "(16x,(a))", " (!Verifique los datos del archivo!)"
	   print "(//)"
       stop
   end if
 end subroutine ver_valores_n_no_nulos
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*******************************VERIFICAR LISTA************************************************* 
!DESCRIPCIóN: verifica que los datos de entrada cumplan con varios aspectos, que garanticen
!el buen desarrollo del programa, en este caso solo se verificará que los índices de fila 
!y de columna correspondientes a la matriz densa sean positivos y la lista no contenga velores
!nulos  
subroutine verificar_dat_entrada(list)
implicit none
!entrada
type(cabecera), intent(in)::list 
!punteros a lista
type(lista), pointer :: lista, actual
!salida
lista=>list%head !apuntamos al primer elemnto de la lista
actual=>lista
do while(associated(actual))
   if (actual%indice_fila<=0 .or. actual%indice_columna<=0) then !
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
       print "(10x,(a))", "    LOS INDICES DE COLUMNA Y DE FILA DEBEN SER POSITIVOS  " 
       print "(16x,(a))", "            (!Verifique los datos del archivo!)"
	   print "(//)"
       stop
   end if
   if (actual%valor==0) then !
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
       print "(10x,(a))", "   NO DEBEN HABER ELEMENTOS DE VALOR NULO EN LA LISTA  " 
       print "(16x,(a))", "            (!Verifique los datos del archivo!)"
	   print "(//)"
       stop
   end if
   actual=>actual%sig !pasamos a la siguiente casilla de la lista
end do
nullify(actual, lista)!liberamos memoria
end subroutine verificar_dat_entrada
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!FIN DE FUNCIÓN LECTURA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end function lectura !fin de funcion lectura

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*************************************SUMA LISTA***********************************************
!SUMA dos listas (listas que representan matrices cuadradas, ambas con las mismas dimensiones) 
function suma_lista(list1, list2)result(lista3)
implicit none
!entrada
type(cabecera), intent(in)::list1,list2
!salida
type(lista), pointer :: lista3
!variables auxiliares, apuntadores a lista, que permitiran movernoa a través de la lista
type(lista), pointer :: lista1, lista2, anterior, actual
!Otras variables: h: si este valor es igual a cero, significa que aún no se han ingresado los
!datos de la primera casilla de la lista.
!i,j serán usados en el bucle de la suma
!n es la dimensión de la matriz que se leerá desde archivo
integer:: h=0, i,j, n
real:: val1, val2 !variables que contrendran el valor del elemnto no nulo que
!se encuentra en la posición i,j
!obtengo el valor de n que guarde en un archivo con su mismo nombre
open(unit=1, file="n.txt",status="old", action="read") !abro el archivo que contiene la !
!dimensión de la matriz
read(1,*),n !leo n
close(1) !cierro el archivo

!apunto a la primera casilla de la lista
lista1=>list1%head
lista2=>list2%head

do i=1,n
 do j=1,n
   val1=encontrar_elem(list1, i, j) !busco el valor de A(i,j)
   val2=encontrar_elem(list2, i, j)
   if (val1/=0 .or. val2/=0) then !si uno o ambos son diferentes de cero lo agrego
   !a mi nueva lista
       if (h==0) then !primera casilla de la lista, donde lista3 apuntara a esta casilla
	   !para luego poder acceder a ella, cuando sea necesario
	      allocate(lista3) !asigno memoria
	      lista3%valor=val1+val2 !asigno los valores correspondientes a valor, e indice columna,
		                         !fila
          lista3%indice_fila=i
          lista3%indice_columna=j
          anterior=>lista3 !paso a la siguiente casilla
		  h=1 !asigno h=1 para que ya no se pueda acceder buevamente a este if, pues ya fue
		  !creada la primera casilla
	   else !a partir del segundo elemento de la lista...
	      allocate(actual)
		  anterior%sig=>actual 
		  actual%valor=val1+val2 !se asigna la suma a la correspondiente casilla junto con los 
          actual%indice_fila=i   !indices
          actual%indice_columna=j
		  anterior=>actual
		  nullify(actual) !libero memoria
	   end if
    end if
  end do
end do
anterior%sig=>NULL() !último elemento con apuntador nulo 
nullify(anterior)!!!!libero memoria

end function suma_lista !FIN DE LA FUNCIÓN SUMA LISTA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!********************************* RESTA LISTA ********************************************
!RESTA DOS LISTA: usa las funciones multiplicar lista por un escalar (ver abajo) y suma de 
!listas (ver función anterior)
!(listas que representan matrices cuadradas, ambas con las mismas dimensiones)
function resta_lista(list1, list2)result(lista3)
implicit none
!ENTRADA
type(cabecera), intent(in)::list1,list2
type(cabecera)::list !variable auxiliar
type(lista), pointer :: lista1, lista2, lista3 !---> lista3 variable de salida
type(lista), pointer :: actual

real:: escalar=-1.0 !escalar de valor negativo


lista1=>list1%head !apuntamos a la primera casilla de la lista
lista2=>list2%head
actual=>escalar*list2 !multiplicamos la lista 2 por menos 1 (valor del escalar)
list%head=>actual !apuntamos a la primera casilla de la lista (variable tipo cabecera)
!para poder hacer la suma, que en realidad serà resta, puesto que list contendra el signo
!(-) menos
lista3=>list1+list !lista3 contendra los valores resultantes de la resta dentro de la lista3
!(nueva)


end function resta_lista !fin función resta
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************MULTIPLICACIÓN LISTA POR VECTOR***********************************************
!Es una especia de multiplicación de matriz por vector, que devuelve un vector
function mult_list_por_vector(list, b)result(vector)
implicit none
!entrada
type(cabecera), intent(in)::list
real, dimension(:), intent(in) :: b
!variable auxiliar
type(lista), pointer :: lista
!salida cuaya dimensión es la misma de b
real, dimension(size(b)) ::vector
integer:: i,j, n !n tamaño del vector b. i,j los indices
real:: val,suma !val: valor del elemnto i,j

n=size(b) !tamaño del vector b
lista=>list%head !apunto al principio de la lista

do i=1,n
 suma=0.0 !inicialización
 do j=1,n
      val=encontrar_elem(list, i, j) !encuentro el valor i,j de la lista y luego
	  suma=suma+val*b(j) !lo multiplico por b(j)
 end do
 vector(i)=suma !vector solución
end do

end function mult_list_por_vector !fin de la FUNCION mult_list_por_vector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!**********************************MULTIPLICAR LISTA ************************************
!multiplicar listas que representan matrices cuadradas, ambas con las mismas dimensiones
function mult_lista(list1, list2)result(lista3)
implicit none
!entrada
type(cabecera), intent(in)::list1,list2
!salida
type(lista), pointer :: lista3
!variables auxiliares
type(lista), pointer :: lista1, lista2, anterior, actual
integer:: h=0, i,j,k, n
real:: val1, val2, suma

!obtengo el valor de n que guarde en un archivo con su mismo nombre
open(unit=1, file="n.txt",status="old", action="read")
read(1,*),n
close(1) !cierro el archivo de solo lectura
lista1=>list1%head !apunto a la primera casilla de la lista
lista2=>list2%head

do i=1,n
 do k=1,n
   suma=0.0 !inicializo la variable real suma a cero
   do j=1,n
      val1=encontrar_elem(list1, i, j) !busco los elemntos correspondientes a los indices i,j 
      val2=encontrar_elem(list2, j, k) !en la lista
	  suma=suma+val1*val2 ! multiplicación
   end do
   if (suma/=0) then !siempre y cuando la suma sea diferente de cero, asignamos 
       if (h==0) then !los valores a las casillas de la nueva lista
	      allocate(lista3) !nuevamente usamos h, para diferenciar la primera casilla
	      lista3%valor=suma ! de las demás
          lista3%indice_fila=i
          lista3%indice_columna=k
          anterior=>lista3
		  h=1
	   else !a partir de la segunda casilla (todo esto sigue la misma idea de la 
	   !funcion sumar_lista)
	      allocate(actual)
		  anterior%sig=>actual
		  actual%valor=suma      !asignamos los valores correspondientes a la casilla
          actual%indice_fila=i
          actual%indice_columna=k
		  anterior=>actual
		  nullify(actual)
	   end if
    end if
  end do
end do
anterior%sig=>NULL()
nullify(anterior)!!!

end function mult_lista !fin función mult_lista
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************MULTIPLICACIÓN POR UN ESCALAR *************************
!Multiplicación de una lista por un escalar
function mult_escalar(escalar, list)result(lista3)
implicit none
!entrada
type(cabecera), intent(in)::list
real, intent(in)::escalar
!lista3. salida
type(lista), pointer :: lista, lista3, anterior, actual
integer :: h=0

lista=>list%head
do while(associated(lista))
       if (h==0) then       !la misma idea usada en sumar lista y mult_lista (ver arriba)
	      allocate(lista3)
	      lista3%valor=escalar*lista%valor !se multiplca por un escalar cada elemento de la lista
          lista3%indice_fila=lista%indice_fila
          lista3%indice_columna=lista%indice_columna
          anterior=>lista3
		  h=1
	   else
	      allocate(actual)
		  anterior%sig=>actual !se multiplica por un escalar cada elemento de la lista
		  actual%valor=escalar*lista%valor
          actual%indice_fila=lista%indice_fila
          actual%indice_columna=lista%indice_columna
		  anterior=>actual
		  nullify(actual)
	   end if
lista=>lista%sig
end do
anterior%sig=>NULL()
end function mult_escalar !fin función mult_escalar


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************** COMPARACIÓN ****************************************
!compara dos lista, con el objetivo de indicar si estas son iguales o diferentes
function comparacion(list1, list2)result(iguales)
implicit none
!entrada
type(cabecera), intent(in)::list1,list2
!apuntadores de tipo lista
type(lista), pointer :: lista1, lista2
!aqui se usarà la función  elemntos no_nulos, para determinar el número de elemntos de cada lista
!no_nulos1 para la lista 1 y no_nulos2 para la lista 2
integer :: no_nulos1, no_nulos2
!salida
logical::iguales
!apunto a la primera casilla de la lista
lista1=>list1%head
lista2=>list2%head
iguales=.false. !inicializo la variable logica a false,
!es decir, si no se cumplen la siguientes indicaciones, las listas no son iguales

no_nulos1=elem_no_nulos(list1) !llamo a la función cantidad de elemntos no nulos
no_nulos2=elem_no_nulos(list2)
if (no_nulos1==no_nulos2) then !si tienen la misma cantidad de elemntos, se procede a verificar
  do while(associated(lista2))
    do while(associated(lista1)) !si el valor y los indices de cada elemento de la lista
	!son iguales
     if (lista1%valor==lista2%valor .and. lista1%indice_fila==lista2%indice_fila .and.lista1%indice_columna==lista2%indice_columna) then
     iguales=.true. !entonces si sin iguales!!!!!!
	 end if
    lista1=>lista1%sig !me muevo a la siguiente casilla de la lista
    end do
  lista2=>lista2%sig !me muevo a la siguiente casilla de la lista
  lista1=>list1%head  !apunto nuevamente a la cabeza de la lista 1, para empezar la comparación
  !otra vez
  end do
end if
end function comparacion !fin de la función comparación

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************EXPONENCIACIÓN DE LISTAS *************************
!EXPONENCIACIÓN de una lista por un valor
function exponenciacion(list,potencia)result(lista3)
implicit none
!entrada
type(cabecera), intent(in)::list
real, intent(in)::potencia
!lista3. salida
type(lista), pointer :: lista, lista3, anterior, actual
integer :: h=0

lista=>list%head
do while(associated(lista))
       if (h==0) then       !la misma idea usada en sumar lista y mult_lista (ver arriba)
	      allocate(lista3)
	      lista3%valor=lista%valor**potencia 
          lista3%indice_fila=lista%indice_fila
          lista3%indice_columna=lista%indice_columna
          anterior=>lista3
		  h=1
	   else
	      allocate(actual)
		  anterior%sig=>actual 
		  actual%valor=lista%valor**potencia
          actual%indice_fila=lista%indice_fila
          actual%indice_columna=lista%indice_columna
		  anterior=>actual
		  nullify(actual)
	   end if
lista=>lista%sig
end do
anterior%sig=>NULL()
end function exponenciacion !fin función mult_escalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module operaciones !fin modulo