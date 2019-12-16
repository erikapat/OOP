
!MODULO LEO MATRIZ DENSA

!LECTURA POR ARCHIVO DE MATRICES

module leo_matriz_densa

implicit none

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!********************************* LECTURA **********************************************
!LECTURA DE MATRICES DENSAS

function lectura_matriz_densa(arch_data)result(matriz_densa)
!entrada
character(len=*), intent(in):: arch_data
!integer, intent(in):: n
!salida 

!real, dimension(n,n) ::matriz_densa
real, dimension(:,:), pointer ::matriz_densa
real, dimension(:),allocatable:: b !vector b
integer::i, j,ios,n
real::elemento
 
open(unit=1, file=arch_data,status="old", action="read", iostat=ios)
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
open(unit=2, file="n.txt",status="unknown", action="write", iostat=ios)
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
open(unit=3, file="b.txt",status="unknown", action="write",iostat=ios)!abro archivo llamado
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
!!!!
!LISTA 1
read(1,*) n !----->guardamos esta variable en un archivo, al cual se pueda acceder cada vez que
!sea necesario
write(2,*)n !----->guardo esta variable en un archivo con su mismo nombre, para acceder
!fácilmente a ella en cualquier momento
call ver_valores_n(n)
! LEE PRIMERA MATRIZ  


allocate(matriz_densa(n,n), b(n))
do i=1,n
   do j=1,n !dimensión de la matriz, note que la matriz es cuadrada
      read(1,*) elemento
	  matriz_densa(i,j)=elemento
   end do
end do


read(1,*,iostat=ios)b(1:n)
call error(ios) !errores al cargar los datos, se detiene el programa en caso de existir error
write(3,*)b(1:n) !guardo en archivo en vector b

close(1)
close(2)
close(3)
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
!1.- n debe ser mayor o igual a 1, (debe quedar claro que usar este tipo de 
!programa para una lista de 1 elemento resulta ineficiente)
 subroutine ver_valores_n(n)
 implicit none
 integer, intent(in):: n
    if (n<=0) then 
       print "(/)"
       print "(11x,(a))", "________________________ERROR_____________________________"
       print "(11x,(a))","( el valor de n y no_nulos debe ser mayor estricto a cero  )"
	   print "(//)"
       stop
    end if
 end subroutine ver_valores_n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!FIN DE FUNCIÓN LECTURA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end function lectura_matriz_densa !fin de funcion lectura


end module leo_matriz_densa
