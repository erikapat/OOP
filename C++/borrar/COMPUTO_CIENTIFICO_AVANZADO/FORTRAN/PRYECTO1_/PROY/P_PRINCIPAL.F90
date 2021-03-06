
                                !P_principal
!Este programa principal resuelve varios sistemas lineales mediante el m�todo iterativo 
!de gauss-Seidel, se cargan los archivos con el formato esparcido, es decir, solo se
!cargaran los elementos no nulos de la matriz densa con sus respectivos �ndices de fila
!y columna   
program P_principal
!modulos que usa este programa
use operaciones  !en este modulo se definen las operaciones que se pueden realizar
!con listas, suma, resta, multiplicaci�n (por otra lista, por un escalar, o por un
!vector), exponenciaci�n, comparaci�n, y lectura de datos
use sistemas_lineales !aqu� se encuentran las funciones necesarias para la resoluci�n de
!los sistemas lineales mediante el m�todo de gauss-seidel
use conversion !este modulo implementa la conversi�n entre el tipo almacenamiento definido
!en el modulo operaciones (almacenamiento en listas) y el almacenamiento denso de matrices
implicit none


type(lista), pointer:: lista1
type(cabecera)::list1
character(len=25):: centinela="1",arch_data1 !nombre del archivo, que no debe superar los 25
integer::ios, i,j, no_nulos, n 

type(lista), pointer::primero,actual, anterior


real::tol
real, dimension(:), allocatable ::b, vector, verificar_x
real, dimension(:,:),allocatable :: matriz_densa



!inicio del ciclo while (centinela)
do while (centinela=="1")
   print "(/)"! ESPACIO
   print "(//,12x,(a))", "*********************************************"
   print "(12x,(a))", "*                                            *"
   print "(12x,(a))", "*                ---  MENU  ---              *"
   print "(12x,(a))", "* OPERACIONES:                               *"
   print "(12x,(a))", "*       0.- SALIR                            *"
   print "(12x,(a))", "*       1.- Resolucion de sistemas mediante  *"
   print "(12x,(a))", "*              GAUSS -SEIDEL                 *"
   print "(12x,(a))", "*                                            *"
   print "(12x,(a))", "**********************************************"
   print "(/)"! ESPACIO
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   print "(16x,(a),\)", "SELECIONE UNA OPCION: "
   read *,centinela
   print "(//)"! ESPACIO
   if (centinela=="0") then
      print "(16x,(a),\)", "FIN DE PROGRAMA " !!------------->fin de programa
	  exit !---->salir
   else if (centinela/="1") then
      print "(16x,(a),\)", "OPCION NO VALIDA " !--------------->opci�n no v�lida
	  print "(/)"
	  centinela="1"
      cycle !---------------------> no se ejecutan las instrucciones siguientes
	  !se imprime nuevamente el menu para que el usuario teclee la opci�n correcta
   end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !Se introduce un nombre de archivo no mayor a 25 caracteres
   print "(16x,(a),\)", "Nombre de Archivo (con su extensi�n): " 
   read *,arch_data1 
   lista1=>.lectura.arch_data1 !!!!!LECTURA
   print "(/)"! ESPACIO
   print "(16x,(a),\)", "INTRODUZCA LA TOLERANCIA: " 
   read *,tol 

   !LEO EL valor que hab�a guardado
   open(unit=1, file="n.txt",status="old", action="read", iostat=ios)
   read(1,*),n
   close(1) !cierro el archivo de solo lectura

   !asigno memoria din�mica
   allocate(matriz_densa(n,n))
   allocate(b(n),vector(n))

   !leo el vector b que habia guardado cuando cargue los datos desde el m�dulo operaciones
   open(unit=1, file="b.txt",status="old", action="read",  iostat=ios)
   read(1,*), b(1:n)
   close(1)!cierro el archivo de solo lectura


   list1%head=>lista1
   print "(16x,(a),/)", "SISTEMA A RESOLVER: " 
   print "(16x,(a),/)", "MATRIZ DENSA: " 

!CONVERSI�N DE ESTRUCTURA: se imprime la matriz densa a partir de la estructura lista
   matriz_densa=conversion_a_matriz_densa(list1)
   do i=1,n
      print "(16x,(a),\)",""
      print*, matriz_densa(i,:) !Imprime por fila
   end do
   !IMPRIMO b
   print "(/)"! ESPACIO
   print "(16x,(a),/)", "LADO DERECHO, b: " 
   print "(16x,(a),\)",""
   print*,b
   print "(//)"! ESPACIO
  
   
   vector=gauss_seidel(list1,b,tol) !vector soluci�n
    !!!verificaci�n
   allocate(verificar_x(n))
  verificar_x=list1*vector
   do i=1,n
     if(verificar_x(i)/=b(i)) then
     print"(16x,(a),/)","VERIFICACI�N FALLO" ! una peque�a diferencia en los decimales 
     end if !arrojar� un error (se debio programar una funci�n para este paso)
   end do
   print "(//)"! ESPACIO
   !RESOLUCI�N DEL SISTEMA LINEAL
   print "(16x,(a),/)", "VECTOR SOLUCION :"
   print "(16x,(a),\)",""
   print*, vector
   print "(//)"! ESPACIO
   pause"              PRESIONE ENTER PARA CONTINUAR" !presionar enter, para continuar
   deallocate(verificar_x, b, matriz_densa, lista1, vector)
end do !fin del ciclo while (centinela)


!******************************************************************************************


end program P_principal
