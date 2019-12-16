!No funciona
program matriz_sparse_listas


!Este programa genera una lista enlazada para almacenar una matriz esparcida.
!Despues de almacenar la matriz, se guarda	 dicho resultado en una 
!matriz densa con el único fin de verificar que todo resultó como se previó. 
!Finalmente, se imprime la matriz en un archivo.

  integer:: i, n, nonulos, indf, indc,ios

!Definición de la estructura de tipo entrada,
!=== que albergará cada una de las entradas
!no nulas de la matriz.

type entrada 
     real valor
     integer indice_fila, indice_columna 
     type (entrada) , pointer :: siguiente
end type entrada

!Esta matriz dinámica sólo se utiliza para que quede claro el funcionamiento del
!programa, pero está claro que es
!=== ilógico usar una matriz densa cuando
!se está tratando de explotar la estructura
!esparcida de la matriz
real, dimension(:,:), allocatable :: a
type (entrada) ,pointer :: primero,actual,anterior


!~== Se abre el archivo donde están contenidos los valores
!=== no nulos de la matriz. La estructura del archivo es
!la siguiente:
!n	                                (orden de la matriz)
!nonulos	                        (cantidad de entradas no nulas de la !matriz)
!valor, indice_fila indice_columna	(por cada entrada no nula de la matriz, existe 
                                    !una línea del archivo con tres valores, a saber, 
									!valor de la entrada, número de la fila y número 
									!de la columna donde está esa entrada en la matriz)

!open(unit=1,file="data.txt",status="old",position="rewind", action="read", err=99)
open(unit=1,file="data.txt",status="old", action="read", err=99)

read (1, * ) n
read(1, *)nonulos
	print*, "n = " ,n, " --- nonulos= "	,nonulos
if (n<=1 .or. nonulos<=1) stop "Datos incorrectos"


!Se comienza a rellenar el conjunto de estructuras por cada entrada de la matriz. 
!Se !leen los datos desde el archivo 'matsparse'.
allocate (primero)
read(1, *,iostat=ios)primero%valor, primero%indice_fila, primero%indice_columna 
if (ios/=0) stop "ERROR"
anterior => primero

do i=1,nonulos
   allocate (actual)

   if (i/=nonulos) then
      anterior%siguiente=>actual
      else
      anterior%siguiente=>NULL()
      exit
   end if
  
   read(1, *)actual%valor,actual%indice_fila,actual%indice_columna 
   anterior => actual
   nullify (actual)
enddo

nullify(anterior)

close (1)


open(unit=2, file='matdensa.txt',status="unknown",position="rewind", & 
     action="write", err=99)

!=== Se almacenan las entradas de la matriz en el arreglo bidimensional a, 
!a partir de la lista enlazada definida y rellenada anteriormente.
allocate(a(n,n))
indf = primero%indice_fila 
indc = primero%indice_columna 
a(indf,indc) = primero%valor 
actual=>primero

do while(associated(actual%siguiente)) 
actual=>actual%siguiente
indf = actual%indice_fila
 indc = actual%indice_columna 
a(indf,indc) = actual%valor
enddo


!Se imprime la matriz densa en el archivo 'matdensa' a fin de apreciar la estructura 
!esparcida de la de la matriz leída. Esto permite verificar el resultado.
do i=1,n
   write(2,*) (a(i,j),j=1,n)
enddo

close (2)

!Liberación de espacio de memoria
nullify(actual,primero)
deallocate (a)

!1 === Esta instrucción 'stop' es fundamental para evitar que se imprima un mensaje de error 
!cuando la ejecución ha sido exitosa.
stop

!Mensaje de error cuando no se puede abrir alguno de los archivos.
99 print *, "Error tratando de abrir el archivo"
end program matriz_sparse_listas
