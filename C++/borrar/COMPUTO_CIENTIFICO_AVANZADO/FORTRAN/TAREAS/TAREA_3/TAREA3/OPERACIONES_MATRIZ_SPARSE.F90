

! Programa principal

program operaciones_matriz_sparse

use operaciones !modulo
implicit none

type(matriz_sparse) :: matriz1, matriz2
type(matriz_sparse) :: matriz3
real :: escalar
integer :: opcion, m_opcion, aux=-1, m, a=-1, a2 !variables auxiliares
!opcion : variable que indica la opción escogida por el usuario
!m_opcion: variable que indica la opción de matriz escogida por el usuario, la
!matriz 1 o la matriz2
!aux mientras esta se negativa se entrara a un ciclo while que contine
!todas las instrucciones del prog principal
!a y a2 serviran para que se le pregunta al usuario, despues de la segunda iteración,
!si usará un archivo distinto al usado inicialmente (ver mas adelante)
logical :: iguales !para verificar si dos matrices son iguales o no 
character(len=25):: arch_data !nombre del archivo, que no debe superar los 25
!carácteres
!se abre un archivo para escribir todos los resultados, de las operaciones
!que realice el usuario 
open(unit=2, file="resultados.txt",status="unknown", action="write")
write(2,"(/,16x,(a))") ""
!MENÚ
do while (aux<0)
print "(//,12x,(a))", "************************************************************"
print "(12x,(a))", "*                                                          *"
print "(12x,(a))", "*                ---  MENU  ---                            *"
print "(12x,(a))", "* OPERACIONES:                                             *"
print "(12x,(a))", "*        0.- SALIR                                         *"
print "(12x,(a))", "*        1.- SUMA DE MATRICES                              *"
print "(12x,(a))", "*        2.- RESTA DE MATRICES                             *"
print "(12x,(a))", "*        3.- MULTIPLICACION DE MATRICES                    *"
print "(12x,(a))", "*        4.- MULTIPLICACION DE UNA MATRIZ POR UN ESCALAR   *"
print "(12x,(a))", "*        5.- COMPARACION                                   *"
print "(12x,(a))", "*        6.- ASIGNACION                                    *"
print "(12x,(a))", "*                                                          *"
print "(12x,(a))", "*                                                          *"
print "(12x,(a))", "************************************************************"
print "(/)"! ESPACIO

!abrir archivo:
print "(16x,(a),\)", "SELECIONE UNA OPCION: "
read *,opcion 

select case (opcion)
case(:-1,7:) !Si es menor que -1 o mayor de siete
   print "(/)"
   print "(16x,(a))", "____________________ERROR____________________"
   print "(16x,(a))", "             ESTA NO ES UNA OPCION" 
   print "(/)"
   pause "              PRESIONE ENTER PARA CONTINUAR"
   print "(/)"
   aux=-1 
   cycle
case(0) !si opcion es cero: FIN DE PROGRAMA
    print "(/)"
    print "(16x,(a))", "____________________ADIOS____________________"
    print "(16x,(a))", "             FIN DE PROGRAMA" 
	print "(13x,(a))", "Resultado de las operaciones en el archivo resultados.txt " 

    print "(/)"
    aux=0 !para que termine el ciclo
    cycle !no se ejecutan las siguientes instrucciones
end select
!LECTURA DE ARCHIVO DE DATOS 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (a==-2) then !A PARTIR DE LA SEGUNDA ITERACION DEL DO WHILE
print "(10x,(a),\)", "Otro Archivo ??? ----> (-1) si  (otro numero) no: " 
read *,a2
   if (a2==-1) then
      a=-1 !así entrara al if, en dode se le pide al usuario el nombre del
      close(1)!archivo (ver abajo) 
   else    
      a=-2 !en la siguiente iteración, se le preguntarà de nuevo al usuario si quiere 
!usar un archivo nuevo o no
   end if
end if
if (a==-1) then !aqui entra en la primera iteración y en las siguientes si el usuario
!introduce un -1
print "(16x,(a),\)", "Nombre de Archivo (con su extensi¢n): " 
read *,arch_data 
write(2,"(/,16x,(a),(a))") "ARCHIVO : ", arch_data !imprime en un archivo, el archivo usado
call lectura(matriz1, matriz2, escalar, arch_data)! lee datos
call verificar_dat_entrada(matriz1) !verifica si los datos son correctos (ver función)
call verificar_dat_entrada(matriz2)
a=-2 !en la siguiente iteración, se le preguntarà de nuevo al usuario si quiere 
!usar un archivo nuevo o no
end if
print "(/,16x,(a))", "MATRIZ 1"  !imprime la matriz densa por pantalla
call imp_mat_rala(matriz1)       
print "(/,16x,(a))", "MATRIZ 2" 
call imp_mat_rala(matriz2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
select case (opcion)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!opcion 1 : suma de matrices!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(1)
print "(/,16x,(a))", "****** SUMA DE MATRICES  *******"
write(2,"(/,16x,(a))") "****** SUMA DE MATRICES  *******"
print "(/,16x,(a))", "MATRIZ 1 + MATRIZ 2 = " 
matriz3=matriz1+matriz2
call imp_mat_rala(matriz3)
call imprimir_en_archivo(matriz3)!imprime la matriz por archivo, en el formato
                                 ! de tres vectores (ver función en modulo)
print "(/)"
pause "              PRESIONE ENTER PARA CONTINUAR"
!!!!!!!!!!!!!!!!!!!!!!!!!!opcion 2: RESTA DE MATRICES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(2)
print "(/,16x,(a))", "****** RESTA DE MATRICES  *******"
write (2,"(/,16x,(a))") "****** RESTA DE MATRICES  *******"
print "(/,16x,(a))", "MATRIZ 1 - MATRIZ 2 = " 
matriz3=matriz1-matriz2
call imp_mat_rala(matriz3) !IMPRIME POR PANTALLA EN FOMA DE MATRIZ DENSA
call imprimir_en_archivo(matriz3)!imprime la matriz por archivo, en el formato
                                 ! de tres vectores (ver función en modulo)
print "(/)"
pause "              PRESIONE ENTER PARA CONTINUAR" !PAUSA
!!!!!!!!!!!!!!!!!!!!!!!!opcion 3; MULTIPLICACION DE MATRICES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(3) !MULTIPLICACION DE MATRICES Y VECTORES
print "(/,16x,(a))", "****** MULTIPLICACION DE MATRICES  *******"
write(2,"(/,16x,(a))") "****** MULTIPLICACION DE MATRICES  *******"
m=-1
do while (m==-1) !ELIGE POR CUAL MATRIZ QUIERE QUE SE MULTIPLIQUE 
   print "(/,16x,(a),\)", "ELIJA  (1) MATRIZ 1*MATRIZ2   (2) MATRIZ 2*MATRIZ 1 = " 
   read *, m_opcion !LEE OPCIÓN
   if (m_opcion==1) then
      print "(/,16x,(a))", "MATRIZ 1 * MATRIZ 2 = " 
      write(2,"(/,16x,(a))") "MATRIZ 1 * MATRIZ 2 = " 
      matriz3=matriz1*matriz2
      exit
   else if (m_opcion==2) then
      print "(/,16x,(a))", "MATRIZ 2 * MATRIZ 1 = " 
      write(2,"(/,16x,(a))") "MATRIZ 2 * MATRIZ 1 = " 
      matriz3=matriz2*matriz1
      exit
   end if
end do
call imp_mat_rala(matriz3)
call imprimir_en_archivo(matriz3)
print "(/)"
pause "              PRESIONE ENTER PARA CONTINUAR"
!!!!!!!!!!!!!!!!!!!!!!!!!!! opcion 4 :MULTIPLICACION DE UNA MATRIZ POR UN ESCALAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(4)
print "(/,16x,(a))", "****** MULTIPLICACION DE UNA MATRIZ POR UN ESCALAR  *******"
write(2,"(/,16x,(a))") "****** MULTIPLICACION DE UNA MATRIZ POR UN ESCALAR *******"
m=-1
do while (m==-1) !ELIGE POR CUAL MATRIZ QUIERE QUE SE MULTIPLIQUE EL ESCALAR
   print "(/,16x,(a),\)", "ELIJA  (1) MATRIZ 1   (2) MATRIZ 2 = " 
   read *, m_opcion !LEE OPCIÓN
   if (m_opcion==1) then
      print "(/,16x,(a))", " escalar* MATRIZ1 = " 
      write(2,"(/,16x,(a))") " escalar* MATRIZ1 = " 
	  matriz3=escalar*matriz1
      exit
   else if (m_opcion==2) then
      print "(/,16x,(a))", " escalar* MATRIZ2 = " 
      write(2,"(/,16x,(a))") " escalar* MATRIZ2 = " 
	  matriz3=escalar*matriz2
      exit
   end if
end do

call imp_mat_rala(matriz3)!IMPRIME POR PANTALLA
call imprimir_en_archivo(matriz3)!IMPRIME EN ARCHIVO
print "(/)"
pause "              PRESIONE ENTER PARA CONTINUAR"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! opcion 5: COMPARACION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(5)
print "(/,16x,(a))", "************ COMPARACION *************"
write(2,"(/,16x,(a))") "****** COMPARACION *******"
print "(/)"
iguales=(matriz1==matriz2)!COMPARACION
if (iguales==.true.) then
   print "(/,16x,(a))", " SON IGUALES "
   write(2,"(/,16x,(a))"), " SON IGUALES "
   print "(/)"
   pause "              PRESIONE ENTER PARA CONTINUAR"
else 
   print "(/,16x,(a))", " NO SON IGUALES "
   write(2,"(/,16x,(a))"), " NO SON IGUALES "
   print "(/)"
   pause "              PRESIONE ENTER PARA CONTINUAR"
end if
write(2,"(/,16x,(a))") "*******************************"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! opcion 6: ASIGNACION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
case(6)
print "(/,16x,(a))", "************ ASIGNACION *************"
m=-1
!Antes de empezar a calcular verifiquemos que a(i,j)=b(k,h) cumpla con i=k y j=h
call dimension_para_sumaoresta(matriz1, matriz2) !ESTA ES UNA SUBRUTINA PENSADA
!PRINCIPALMENTE PARA LAS OPERACIONES DE SUMA Y RESTA, PERO QUE ES CONVENIENTE
!PARA LA ASIGNACIÓN
do while (m==-1) !CUALES VALORES QUIERE ASIGNAR, LOS DE LA MATRIZ 1 o 2
   print "(/,13x,(a),\)", "!CUALES VALORES QUIERE ASIGNAR, LOS DE LA MATRIZ 1 o 2" 
   print "(/,16x,(a),\)", "ELIJA  (1) MATRIZ 1   (2) MATRIZ 2 = " 
   read *, m_opcion
   if (m_opcion==1) then !si se escoge la 1
      matriz2=matriz1
      call imp_mat_rala(matriz2) !imprime en pantalla
	  write(2,"(/,16x,(a))") "****** ASIGNACION DE MATRICES  *******"
      call imprimir_en_archivo(matriz2) !imprime en archivo
      print "(/)"
	  exit
   else if (m_opcion==2) then !si se escoge la matriz 2
      matriz1=matriz2
	  call imp_mat_rala(matriz2)
	  write(2,"(/,16x,(a))") "****** ASIGNACION DE MATRICES  *******"
      call imprimir_en_archivo(matriz1)
      print "(/)"
      exit
   end if
end do
pause "              PRESIONE ENTER PARA CONTINUAR"
end select
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end do 
close(2) !cerrar archivo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program operaciones_matriz_sparse
