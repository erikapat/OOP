
                               !P_PRINCIPAL2
!Este programa principal resuelve varios sistemas lineales mediante el m�todo iterativo 
!de gauss-Seidel, se cargan los archivos con el formato de matriz densa
program p_principal2
!modulos que usa este programa
use operaciones  !en este modulo se definen las operaciones que se pueden realizar
!con listas, suma, resta, multiplicaci�n (por otra lista, por un escalar, o por un
!vector), exponenciaci�n, comparaci�n, y lectura de datos
use sistemas_lineales !aqu� se encuentran las funciones necesarias para la resoluci�n de
!los sistemas lineales mediante el m�todo de gauss-seidel
use conversion !este modulo implementa la conversi�n entre el tipo almacenamiento definido
!en el modulo operaciones (almacenamiento en listas) y el almacenamiento denso de matrices
use leo_matriz_densa !para cargar los datos en formato de matriz
implicit none


type(lista), pointer:: lista1
type(cabecera)::list1
character(len=25):: centinela="1",arch_data1 !nombre del archivo, que no debe superar los 25
integer::ios, i,j, no_nulos, n 



real:: escalar, val, norm, tol
real, dimension(:), allocatable ::b, vector
real, dimension(:,:), pointer:: matriz_densa


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
   
  

   !LEO EL valor de que hab�a guardado
   open(unit=1, file="n.txt",status="old", action="read", iostat=ios)
   read(1,*),n
   call error(ios) !en caso de no abrirse el archivo o no se pueda leer el contenido se 
   !detien el programa con un mensaje
   close(1) !cierro el archivo de solo lectura
   
   allocate(matriz_densa(n,n))

   matriz_densa=>lectura_matriz_densa(arch_data1) !!!!!LECTURA
   print "(/)"! ESPACIO
   print "(16x,(a),\)", "INTRODUZCA LA TOLERANCIA: " 
   read *,tol 

   allocate(b(n),vector(n))

   !leo el vector b que habia guardado cuando cargue los datos desde el m�dulo operaciones
   open(unit=1, file="b.txt",status="old", action="read",  iostat=ios)
   read(1,*), b(1:n)
   close(1)!cierro el archivo de solo lectura

   print "(16x,(a),/)", "SISTEMA A RESOLVER: " 
   print "(16x,(a),/)", "MATRIZ DENSA: " 

   do i=1,n
      print*, matriz_densa(i,1:n) !Imprime por fila
	  end do
   print "(/)"! ESPACIO
   print "(16x,(a),/)", "LADO DERECHO, b: " 
   print "(16x,(a),\)",""
   print*,b

    !hago la conversi�n
   lista1=>conversion_a_lista(matriz_densa)
   list1%head=>lista1
   print "(//)"! ESPACIO
   !RESOLUCI�N DEL SISTEMA LINEAL
   print "(16x,(a),/)", "VECTOR SOLUCION :"
   vector=gauss_seidel(list1,b,tol)
   print*, vector
   print "(//)"! ESPACIO
   pause"              PRESIONE ENTER PARA CONTINUAR" !presionar enter, para continuar

deallocate(b, vector, matriz_densa)
end do
!fin del ciclo while (centinela)

!******************************************************************************************


contains
!************** DENTRO DE ESTA FUNCI�N TENERMOS 2 SUBPROGRAMAS **********************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SUBPROGRAMA ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DESCRIPCI�N:! funci�n error: si el formato del archivo es diferente al especificado, 
!se produce un error y si no exixte el archivo especificado, o este esta mal escrito
!tambi�n arroja un error
!cualquiera de los dos errores detiene la ejecuci�n programa
!1.- NOMBRE Y FORMATO DE ARCHIVO
 subroutine error(ios)
   implicit none
   integer, intent(in):: ios
   if (ios==29) then !el n�mero 29 indica un error de falla de lectura
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




end program p_principal2