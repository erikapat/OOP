
program P_Principal

use operaciones_basicas

implicit none


type(m_densa) pointer:: puntero
type(lista), pointer :: actual,anterior,primero, punt
real:: elemento, k
integer ::n, ios, i,j, no_nulos,h
character(len=25):: arch_data !nombre del archivo, que no debe superar los 25


print "(16x,(a),\)", "Nombre de Archivo (con su extensi¢n): " 
read *,arch_data 
open(unit=1, file=arch_data,status="old", action="read", iostat=ios)
call error(ios) !!!!OJO HAY QUE BORRARLO

read(1,*,iostat=ios)n
call error(ios)! función error: si el formato del archivo
!es diferente al especificado, se produce un error

!allocate(puntero%fila(n))
no_nulos=0
do i=1,n
  h=0
  allocate(puntero%fila(i))
  do j=1,n
     read(1,*,iostat=ios)elemento
	 if (elemento/=0) then
	    if (h==0) then
           allocate(primero)
		   primero%valor=elemento
           primero%indice_columna=j
           puntero=>primero
		   anterior=>primero
		   h=1
		else
		   anterior%sig=>actual
           allocate(actual)
		   actual%valor=elemento
           actual%indice_columna=j
		   print *, elemento
           print *, actual%valor
		   print *, actual%indice_columna
           anterior=>actual
		   nullify(actual)
		   no_nulos=no_nulos+1
        end if
	 end if   
	 if (j==n) then
        anterior%sig=>NULL()
	 end if
  end do
end do

 ! punt=>puntero%fila(1)
!punt=>primero
print*, punt%valor


contains

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

end program P_Principal

