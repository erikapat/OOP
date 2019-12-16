
                         !MODULO
!DESCRIPCIÓN
!modulo que permite manipular y efectuar operaciones con matrices ralas
!que vienen dadas mediante tres arreglos dinámicos. 
!Las operaciones a implementar son adición y sustracción de dos matrices,
!multiplicación de matrices( de cualquier tamaño) y multiplicación de matrices por un escalar 
module operaciones

   implicit none

   type matriz_sparse
     real, dimension(:), pointer :: valor
     integer, dimension(:), pointer :: IC, ICF
     integer :: fila, columna, no_nulos
   end type matriz_sparse
!LA ESTRUCTURA MATRIZ_SPARSE: 
!Esta estructura contiene tres arreglos uno real y dos enteros, denominados
!valor, IC, ICF, que corresponden a los valores no nulos de la matriz, al
!vector de indices columna y al vector indices columna fila
!Tambien contiene tres enteros correspodientes al número de filas, columnas 
!y número de elementos no nulos 
!---> valor: vector de dimensión no_nulos, que contiene los elementos no nulos de la
!             matriz rala
!---> IC:    vector de dimensión no_nulos, que contiene el número de columnas de cada
!            elemento no nulo del vector valor
!---> ICF:   vector de dimensión fila+1, que contiene el subíndice del elemento valor
!            que encabeza cada fila de la matriz rala 
!

!interfaces
interface operator(+)
 module procedure suma_matriz !suma dos matrices de la misma dimensión
end interface

interface operator(-)
 module procedure resta_matriz !resta dos matrices de la misma dimensión
end interface

interface operator(*)
 module procedure mult_esc_matriz !multiplicación por un escalar
 module procedure mult_matriz     !multiplicaciòn de matrices a(i,j)*b(j,k), donde 
end interface                     ! k e i puden tener valores enteros diferentes           
                                  !            a(i,j)*b(j,k) = c(i,k)

interface operator(==)
 module procedure comparacion_matriz !verifica que dos matrices sea iguales o no,
end interface

interface assignment(=)
 module procedure asigna_matriz !asigna una matriz a otra
end interface
!todas estas operaciones se basan en la estructura matriz_sparse

contains



!******************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FUNCIÒN: LECTURA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: Lee los datos del archivo introducido por el usuario
subroutine lectura(matriz1, matriz2, escalar,arch_data)
!salida
type(matriz_sparse), intent(out):: matriz1, matriz2 !permite acceder a la estructura
real, intent(out) :: escalar
!entrada
character(len=25), intent(in):: arch_data
!variable que servirà`para verificar si se logro abrir o leer el archivo
integer ::ios=0 !en caso de error sera igual a 1
!se abre el archivo
open(unit=1, file=arch_data,status="old", action="read", iostat=ios)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ERROR NO SE ABRE EL ARCHIVO !!!!!!!!!!!!!!!!!!!!!!!!!
if (ios/=0) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", " NO SE PUEDE ABRIR EL ARCHIVO ESPECIFICADO" 
   print "(16x,(a))", " (!Verifique que el archivo a leer exista!)"
   print "(//)"
   stop
end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! LEE PRIMERA MATRIZ  !!!!!!!!!!!!!!!!!!!!!!!!!
read(1,*,iostat=ios) matriz1%no_nulos
call error(ios)! función error: si el formato del archivo
!es diferente al especificado, se produce un error

read(1,*,iostat=ios)matriz1%fila, matriz1%columna !variables fila y columna (en una misma lìnea)
call error(ios)! función error: si el formato del archivo
!es diferente al especificado, se produce un error

!!!vECTOR VALOR
!ASIGNACION DE MEMORIA
allocate(matriz1%valor(matriz1%no_nulos)) !vector valor de longitud no_nulos
read(1,*, iostat=ios) matriz1%valor(1:matriz1%no_nulos) !!!!!
call error(ios)

!!!vECTOR IC
!ASIGNACION DE MEMORIA
allocate(matriz1%IC(matriz1%no_nulos))!vector IC de longitud no_nulos
read(1,*, iostat=ios) matriz1%IC(1:matriz1%no_nulos)  !!!!
call error(ios)

!!!vECTOR ICF
!ASIGNACION DE MEMORIA
allocate(matriz1%ICF(matriz1%fila+1))!vector ICF de longitud fila+1
read(1,*, iostat=ios) matriz1%ICF(1:matriz1%fila+1)  !!!!
call error(ios)

matriz1%no_nulos=size(matriz1%IC)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!LECTURA SEGUNDA MATRIZ !!!!!!!!!!!!!!!!!!!
read(1,*,iostat=ios) matriz2%no_nulos
call error(ios)! función error: si el formato del archivo
!es diferente al especificado, se produce un error

read(1,*, iostat=ios)matriz2%fila, matriz2%columna !variables fila y columna (en una misma lìnea)
call error(ios)! función error: si el formato del archivo
!es diferente al especificado, se produce un error
!!!vECTOR VALOR
!ASIGNACION DE MEMORIA
allocate(matriz2%valor(matriz2%no_nulos)) !vector valor de longitud no_nulos
read(1,*, iostat=ios) matriz2%valor(1:matriz2%no_nulos) !!!!!
call error(ios)
!!!vECTOR IC
!ASIGNACION DE MEMORIA
allocate(matriz2%IC(matriz2%no_nulos)) !vector IC de longitud no_nulos
read(1,*, iostat=ios) matriz2%IC(1:matriz2%no_nulos)  !!!!
call error(ios)
!!!vECTOR ICF
!ASIGNACION DE MEMORIA
allocate(matriz2%ICF(matriz2%fila+1)) !vector ICF de longitud fila+1
read(1,*, iostat=ios) matriz2%ICF(1:matriz2%fila+1)  !!!!
call error(ios)


read(1,*, iostat=ios)escalar !VARIABLE ESCALAR
call error(ios)

close(1) !cierro archivo

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBPROGRAMA ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN:! función error: si el formato del archivo es diferente al especificado, 
!se produce un error
 subroutine error(ios)
   integer, intent(in):: ios
   if (ios/=0) then
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
	   print "(11x,(a))", "                   FALLA DE LECTURA                       " 
       print "(11x,(a))","(Verifique que su archivo cumpla con el formato establecido)"
	   print "(//)"
       stop
   end if
 end subroutine error

end subroutine lectura
!******************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBRUTINA: VERIFICAR DATOS DE ENTRADA !!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: verifica que los datos de entrada verifique varios aspectos, que garanticen
!el buen desarrollo del programa
subroutine verificar_dat_entrada(matriz)
type(matriz_sparse), intent(in):: matriz !permite acceder a la estructura

integer::i
!1.- Que la matriz tenga un nùmero de elemntos no nulos estrictamente mayor que 1
if (matriz%no_nulos<1) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(13x,(a))", "la variable no_nulos debe ser estrictamente mayor que uno" 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
!2.- Que la dimensión de la matriz sea estrictemente mayor que uno
!se permite vectores de dimensiones 1xn ó nx1, matrices de dimensiones nxm
!con n y m iguales o diferentes, pero no se aceptan matrices 1x1, no 
!tendría mucho sentido, para eso se recomienda la opción de multiplicar por un escalar
if (matriz%fila<1) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(13x,(a))", "la variable fila debe ser estrictamente mayor que cero" 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
   

if (matriz%columna<1) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(13x,(a))", "la variable columna debe ser estrictamente mayor que cero" 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if

if (matriz%columna==1 .and. matriz%fila==1) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(13x,(a))", "     MATRIZ DE DIMENSIÓN 1 X 1 NO ACEPTADA   " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
!la variable no_nulos no debe sobrepasar el valor fila*columna
if (matriz%no_nulos>matriz%columna*matriz%fila) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(10x,(a))", "NO PUEDE HABER MAS ELEMENTOS NO_NULOS QUE ELEMENTOS DE LA MATRIZ   " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
!3.- El vector de elementos no nulos y el vector IC deben tener la misma dimensión
if (size(matriz%valor)/=size(matriz%IC)) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "la longitud del vector de valores no nulos debe" 
   print "(16x,(a))", "ser igual a la longitud del vector IC (indice columna)" 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if

!4.- El vector ICF debe tener dimension igual al número de filas +1
if (size(matriz%ICF)/=matriz%fila+1) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "la longitud del vector ICF debe ser igual a la" 
   print "(16x,(a))", "           a la varible fila + 1              " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
!5.- El último elemento de el vector ICF no puede sobrepasar del valor de elementos
!no nulos +1
if (matriz%ICF(matriz%fila+1)>(matriz%no_nulos +1)) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(14x,(a))", "Vector ICF mal definido, el último elemento no puede" 
   print "(16x,(a))", "     ser mayor al número de elementos no nulos +1   " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
end if
!6.- El vector ICF no puede contener elementos menores o iguales a cero
do i=1,matriz%fila+1
 if (matriz%ICF(i)<=0) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "Vector ICF mal definido, no debe tener elementos " 
   print "(16x,(a))", "               nulos ni negativos                " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
 end if
end do
!los valores contenidos en el ICF debe ser todos diferentes y mayores
!a los elementos anteriores del mismo vector
do i=1,matriz%fila
 if (matriz%ICF(i)>=matriz%ICF(i+1)) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "Vector ICF mal definido, no debe tener elementos " 
   print "(16x,(a))", "    iguales o de mayor valor que su sucesor     " 
   print "(16x,(a))", " (!Verifique los datos del archivo!)"
   print "(//)"
   stop
 end if
end do

!7.- El vector de valores no nulos no puede contener elementos nulos
do i=1,matriz%no_nulos
 if (matriz%valor(i)==0) then
   print "(/)"
   print "(12x,(a))", "___________________________ERROR___________________________"
   print "(12x,(a))", "Vector de valores no nulos, no debe contener valores nulos " 
   print "(12x,(a))", "              (!Verifique los datos del archivo!)"
   print "(//)"
   stop
 end if
end do

!8.-El vector IC no debe tener elementos de valor menor o igual a cero o mayor al número
!de columnas
do i=1,matriz%no_nulos
 if (matriz%IC(i)<=0 .and. matriz%IC(i)>matriz%columna) then
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "Vector IC mal definido, no debe tener elementos " 
   print "(16x,(a))", "  nulos, negativos o mayores del valor columna  " 
   print "(16x,(a))", "       (!Verifique los datos del archivo!)      "
   print "(//)"
   stop
 end if
end do

end subroutine verificar_dat_entrada

!******************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBPROGRAMA: IMPRIMIR MATRIZ DENSA !!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: Imprime la correspondiente matriz densa a partir del patron sparse usado
!en la estructura matriz_sparse
subroutine imp_mat_rala(matriz)
type(matriz_sparse), intent(in):: matriz !permite acceder a la estructura
integer :: i,j !indices

real, dimension(:,:), allocatable ::m_densa !definimos una matriz dinámica, como la matriz densa
allocate(m_densa(matriz%fila,matriz%columna))!asignamos memoria
print "(/)" !enter (cambio de línea)
 !creacion de la matriz
 do i=1,matriz%fila
    do j=1,matriz%columna
    m_densa(i,j)=encontrar_elem(matriz, i, j)!busca los elemntos con indice i,j
	end do	
 end do
!IMPRIMIR
   do i=1,matriz%fila
   print "(16x,(a),\)", ""
   print *,m_densa(i,:)!imprime por fila
   end do
!deallocate(m_densa)
   
end subroutine imp_mat_rala
!******************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBRUTINA IMPRIMIR EN ARCHIVO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!imprime la matriz por archivo, en el formato de tres vectores valor, IC e ICF
subroutine imprimir_en_archivo(matriz)
type(matriz_sparse), intent(in):: matriz !permite acceder a la estructura
write(2,"(/,10x,(a))"),"Numero de elementos no nulos: "
write(2,*)matriz%no_nulos
write(2,"(/,10x,(a))")"Numero de filas: "
write(2,*)matriz%fila
write(2,"(/,10x,(a))")"Numero de columnas: "
write(2,*)matriz%columna
write(2,"(/,10x,(a))")"Vector de elementos no nulos: "
write(2,*)matriz%valor
write(2,"(/,10x,(a))")"Vector IC: "
write(2,*)matriz%IC
write(2,"(/,10x,(a))")"Vector ICF"
write(2,*)matriz%ICF
write(2,*)"*****************************************************************"

end subroutine imprimir_en_archivo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!******************************************************************************************!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FUNCIÒN: ENCONTRAR ELEMENTO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: busca en la estructura matriz_sparse, especificamente en el vector valor
!el elemento correspondiente a un indice i,j de la matriz densa, sin necesidad de crear la matriz
!densa original
function encontrar_elem(matriz, i, j)result(val)
implicit none
type(matriz_sparse), intent(in):: matriz !elemento tipo matriz_sparse que nos permite
!acceder a la estructura
integer, intent(in) :: i, j !indices
real :: val !elemento ubicado en a(i,j), siendo a la matriz rala 
integer:: n, h, k, m !variables auxiliares

n=size(matriz%ICF)-1 ! dimensión de el vector ICF
h=1 !servirá para indicar el indice de ICF
val=0 !-----> si no se encuentra el valorde elemento i,j dentro del vector, significa
!que ese elemnto vale 0 
  do k=1,n !fila
     do m=matriz%ICF(k), matriz%ICF(k+1)-1 !en cada fila hay tantos elementos
	    if (i==h .and. j==matriz%IC(m)) val=matriz%valor(m) !así que reviso en esa fila
		!en cada uno de los elementos no nulos si esta el elemneto que busco, si esta lo
		!asigno a val 
	 end do
	 h=h+1
  end do
end function encontrar_elem
!**************************************************************************************
!!!!!!!!!!!!!!!! SUBRUTINA DIMENSIÓN PARA SUMA O RESTA O ASIGNACIÓN!!!!!!!!!!!!!!!!!!!!!!!
subroutine dimension_para_sumaoresta(matriz1, matriz2)
!se verifica la factibilidad de la operación de acuerdo a la dimensión de las matrices
!para que la suma, resta, o la asignación pueda realizarse con exito 
!verifica que a(i,j)+(-)b(k,h) cumpla con i=k y j=h, para operaciones de suma y resta
type(matriz_sparse), intent(in):: matriz1, matriz2 !permite acceder a la estructura
 
 if ((matriz1%fila/=matriz2%fila) .or. (matriz1%columna/=matriz2%columna)) then
   print "(/)"
   print "(16x,(a))", "___________________________ERROR___________________________"
   print "(16x,(a))", "           Las matrices debe tener la misma dimensión " 
   print "(16x,(a))", "              (!Verifique los datos del archivo!)"
   print "(//)"
   stop
 end if
end subroutine dimension_para_sumaoresta
!**************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBRUTINA DIMENSIÓN PARA MULTIPLICACION!!!!!!!!!!!!!!!!!!!!!!!
subroutine dimension_para_mult(matriz1, matriz2)
!verifica que a(i,j)b(h,k)=c(i,k) cumpla con j=h donde i, k puede tener valores diferentes ,
! para operaciones de multiplicacion
type(matriz_sparse), intent(in):: matriz1, matriz2 !permite acceder a la estructura
 if (matriz1%columna/=matriz2%fila) then
   print "(/)"
   print "(16x,(a))", "___________________________ERROR___________________________"
   print "(16x,(a))", " Dimensión de las matrices incorrectas para la multiplicación" 
   print "(16x,(a))", "              (!Verifique los datos del archivo!)"
   print "(//)"
   stop
 end if
end subroutine dimension_para_mult
!**************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIóN SUMAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: suma dos matrices ralas, expresadas como en la estructura matriz_sparse
function suma_matriz(matriz1, matriz2)result(matriz3)
type(matriz_sparse), intent(in):: matriz1, matriz2 !elementos tipo matriz_sparse que nos 
!permite acceder a la estructura
type(matriz_sparse):: matriz3
!variables auxiliares
integer, dimension(:), allocatable :: a_ic, a_icf !vectores para IC e ICF
real, dimension(:), allocatable :: a_valor ! vector VALOR
integer:: n, h, i, j, elem, fil  !!!!!!
real :: val2, val1  !VALORES DEVUELTOS DE LA FUNCIÒN encontrar_elemento
!Antes de empezar a calcular verifiquemos que a(i,j)+b(k,h) cumpla con i=k y j=h
call dimension_para_sumaoresta(matriz1, matriz2)
!asigno memoria
allocate(a_ic(matriz1%fila*matriz1%columna), a_valor(matriz1%fila*matriz1%columna))
allocate(a_icf(matriz1%fila+1))
!iniacilización:
h=1 !h indicara dimensión del vector a_valor y a_ic
fil=1 !fil indicara la dimensión del vector a_icf 
elem=1 !elem servirá para crear el vector a_icf (los elemntos de a_icf)
a_icf(fil)=1 
  do i=1,matriz1%fila !fila
     do j=1, matriz1%columna !columna
	 val2=encontrar_elem(matriz2, i, j) !función encontrar elemento (explicada arriba)
	 val1=encontrar_elem(matriz1, i, j)
	 if (val1+val2==0) cycle !si la suma de los elementos es cero salir no seguir con las
	 !intrucciones siguientes
	 a_valor(h)=val1+val2 !si la suma no es cero asignar a la variable a_valor en la posición h
	 a_ic(h)=j !asigno indice columna
	 elem=elem+1 	 
	 h=h+1
	 end do
	 fil=fil+1
	 a_icf(fil)=elem !asigno indice columna fila
	  
  end do

!asigno memoria
allocate(matriz3%valor(h-1))
allocate(matriz3%IC(h-1))
allocate(matriz3%ICF(fil))
!asigno lo obtenido en los cálculo anteriores a una variable tipo matriz_sparse, 
!pues ya conosco las dimensiones de los vectores
matriz3%fila=matriz1%fila
matriz3%columna=matriz1%columna

matriz3%valor(1:h-1)=a_valor(1:h-1)
matriz3%IC(1:h-1)=a_ic(1:h-1)
matriz3%ICF(1:fil)=a_icf(1:fil)
matriz3%no_nulos=matriz3%ICF(fil)-1

deallocate(a_ic,a_valor,a_icf) !libero memoria

end function suma_matriz


!*****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FUNCIÓN RESTA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCIóN: resta dos matrices ralas, expresadas como en la estructura matriz_sparse, similar
!ala función suma
function resta_matriz(matriz1, matriz2)result(matriz3)
type(matriz_sparse), intent(in):: matriz1, matriz2 
!permite acceder a la estructura
type(matriz_sparse):: matriz3
!variables auxiliares
integer, dimension(:), allocatable :: a_ic, a_icf !!!!!vectores para IC e ICF
real, dimension(:), allocatable :: a_valor !!!!! vector VALOR
integer:: n, h, i, j, elem, fil  !!!!!!
real :: val2, val1  !!!!!!!VALORES DEVUELTOS DE LA FUNCIÒN encontrar_elemento
!Antes de empezar a calcular verifiquemos que a(i,j)+b(k,h) cumpla con i=k y j=h
call dimension_para_sumaoresta(matriz1, matriz2)
!asigno memoria
allocate(a_ic(matriz1%fila*matriz1%columna), a_valor(matriz1%fila*matriz1%columna))
allocate(a_icf(matriz1%fila+1))
!h indicara dimensión del vector a_valor y a_ic
!fil indicara la dimensión del vector a_icf
!elem servirá para crear el vector a_icf (los elemntos de a_icf)
h=1
fil=1
elem=1
a_icf(fil)=1
  do i=1,matriz1%fila
     do j=1, matriz1%columna
	 val2=encontrar_elem(matriz2, i, j)!función encontrar elemento (explicada anteriormente)
	 val1=encontrar_elem(matriz1, i, j)
	 if (val1-val2==0) cycle !si la resta de los elementos es cero salir no seguir con las
	 !intrucciones siguientes
	 !asignaciones
	 a_valor(h)=val1-val2 !si la suma no es cero asignar a la variable a_valor en la posición h
	 a_ic(h)=j
	 elem=elem+1	 
	 h=h+1
	 end do
	 fil=fil+1
	 a_icf(fil)=elem !asigno indice columna fila
	  
  end do

allocate(matriz3%valor(h-1))
allocate(matriz3%IC(h-1))
allocate(matriz3%ICF(fil))
!asigno lo obtenido en los cálculo anteriores a una variable tipo matriz_sparse, 
!pues ya conosco las dimensiones de los vectores

matriz3%fila=matriz1%fila
matriz3%columna=matriz1%columna

matriz3%valor(1:h-1)=a_valor(1:h-1)
matriz3%IC(1:h-1)=a_ic(1:h-1)
matriz3%ICF(1:fil)=a_icf(1:fil)
matriz3%no_nulos=matriz3%ICF(fil)-1

deallocate(a_ic,a_valor,a_icf)


end function resta_matriz

!****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!MULTIPLICACIÓN POR UN ESCALAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRICIÓN: multiplica una matriz de tipo matriz-sparse por un escalar de tipo real
function mult_esc_matriz(escalar, matriz1)result(matriz3)
implicit none
!entrada
type(matriz_sparse), intent(in):: matriz1 !permite acceder a la estructura
real, intent(in) :: escalar
!salida!
type(matriz_sparse):: matriz3 !permite acceder a la estructura
!asigno los valores de no_nulos, columna y fila
matriz3%no_nulos=matriz1%no_nulos
matriz3%columna=matriz1%columna
matriz3%fila=matriz1%fila
!asigno memoria
allocate(matriz3%valor(matriz1%no_nulos))
allocate(matriz3%IC(matriz1%no_nulos))
allocate(matriz3%ICF(matriz1%fila+1))
!multiplicaciòn por un escalar
matriz3%valor=escalar*matriz1%valor
matriz3%IC=matriz1%IC
matriz3%ICF=matriz1%ICF


end function mult_esc_matriz
!****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MULTIPLICACION MATRICES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRICIÓN: multiplica matrices a(i,j)b(j,k)=c(i,k) donde i, k puede tener valores diferentes
function  mult_matriz(matriz1, matriz2)result(matriz3)
!entrada
type(matriz_sparse), intent(in):: matriz1, matriz2 
!salida
type(matriz_sparse):: matriz3 !permite acceder a la estructura
!variables auxiliares
integer, dimension(:), allocatable :: a_ic, a_icf !!!!!!!!!vectores para IC e ICF
real, dimension(:), allocatable :: a_valor !!!!!!!!!vectores para valor
integer:: i, j, k, fil, h, suma, elem  !!!!!!
real :: val2, val1  !!!!!!!!!!!!!VALORES DEVUELTOS DE LA FUNCIÓN encontrar_elemento
!verifica que las dimensiones de las matrices sean las correctas para la multiplicación
call dimension_para_mult(matriz1, matriz2)
!la dimnsión maxima de los vectores será (suponiendo a(i,j)b(i,k)
!asigno memoria
allocate(a_ic(matriz1%fila*matriz2%columna), a_valor(matriz1%fila*matriz2%columna))
allocate(a_icf(matriz1%fila+1))
!h indicara dimensión del vector a_valor y a_ic
!fil indicara la dimensión del vector a_icf
!elem servirá para crear el vector a_icf (los elemntos de a_icf)
!inicialización
h=1
fil=1
a_icf(fil)=1
elem=1
  do i=1,matriz1%fila
     do k=1,matriz2%columna
	    suma=0
        do j=1, matriz1%columna
	    val1=encontrar_elem(matriz1, i, j)!VALORES DEVUELTOS DE LA FUNCIÒN encontrar_elemento
		val2=encontrar_elem(matriz2, j, k)
		suma=suma+val1*val2 !suma =suma+a(i,j)b(i,k) => c(i,k)=suma
		end do
	 a_valor(h)=suma
	 a_ic(h)=k
	 if (suma==0) cycle !salta la instrucciones siguientes
	 elem=elem+1
	 h=h+1
	 end do
	 fil=fil+1
	 a_icf(fil)=elem	  
  end do
!asigno memoria
allocate(matriz3%valor(h-1))
allocate(matriz3%IC(h-1))
allocate(matriz3%ICF(fil))
!asigno a matriz3, salida
matriz3%fila=matriz1%fila
matriz3%columna=matriz2%columna

matriz3%valor(1:h-1)=a_valor(1:h-1)
matriz3%IC(1:h-1)=a_ic(1:h-1)
matriz3%ICF(1:fil)=a_icf(1:fil)
matriz3%no_nulos=matriz3%ICF(fil)-1
matriz3%no_nulos=matriz3%ICF(fil)-1
!libero memoria
deallocate(a_ic,a_valor,a_icf)

end function mult_matriz
!****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!COMPARACION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRICIÓN: verifica si el pantron sparse introducido es el de dos matrices indenticas
function comparacion_matriz(matriz1, matriz2)result(mat)
!entrada
type(matriz_sparse), intent(in):: matriz1, matriz2 !permite acceder a la estructura

!salida
logical:: mat
integer ::i
mat=.true. !supone de antemano que son iguales
if (matriz1%no_nulos==matriz2%no_nulos .and. matriz1%fila==matriz2%fila &
    .and. matriz1%columna==matriz2%columna) then !primero verifico que no_nulos, 
	!fila y columna sean iguales
	do i=1,matriz1%no_nulos !verifica si los vectores valor e IC son iguales
	   if ((matriz1%valor(i)/=matriz2%valor(i)).or.(matriz1%IC(i)/=matriz2%IC(i))) mat=.false.
	end do
	if (mat/=.false.) then !verifica si los vactores ICF son iguales
       do i=1,matriz1%fila+1
	      if (matriz1%ICF(i)/=matriz2%ICF(i)) mat=.false.
	   end do
	end if
else
    mat=.false.
end if

end function comparacion_matriz
!****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ASIGNACION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRICIÓN: asigna los valores de una matriz del tipo matriz_sparse, a otra matriz del
!mismo tipo
!OJO:subrutinas para las funciones de module operator no funcionan para subrutinas
!module assigment, el compilador no da error, pero crea conflictos y salidas erroneas
subroutine asigna_matriz(matriz1, matriz2)
!entrada-salida
type(matriz_sparse), intent(out):: matriz1 !permite acceder a la estructura
type(matriz_sparse), intent(in):: matriz2
!verifico que posean la misma dimensión

!asigno los valores de no_nulos, columna y fila
matriz1%no_nulos=matriz2%no_nulos
matriz1%columna=matriz2%columna
matriz1%fila=matriz2%fila
!asigno memoria
allocate(matriz1%valor(matriz2%no_nulos))
allocate(matriz1%IC(matriz2%no_nulos))
allocate(matriz1%ICF(matriz2%fila+1))
!asigno lo restante
matriz1%valor=matriz2%valor
matriz1%IC=matriz2%IC
matriz1%ICF=matriz2%ICF

end subroutine asigna_matriz

!****************************************************************************************

end module operaciones