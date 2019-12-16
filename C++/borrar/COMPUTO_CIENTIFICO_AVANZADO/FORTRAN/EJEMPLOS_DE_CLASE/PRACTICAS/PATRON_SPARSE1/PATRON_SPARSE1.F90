program patron_sparse1

implicit none
real::	valor
integer::	i,j,nfilas,ncolumnas
real, dimension(:,:), allocatable::	matriz
character(len=2), dimension(:,:),  allocatable :: patron 
character(len=20):: arch_matriz, arch_patron

!	Lee los nombres de los archivos por pantalla

print*, "Nombre archivo matriz :" !Nombre con extensión
read*,arch_matriz
open(unit=1, file=arch_matriz, status="old", action="read", &
     position= "rewind",err=99)

print*,"Nombre archivo patron :"!Nombre con extensión
read*,arch_patron
open	(unit=2, file=arch_patron, status="unknown", action="write", & 
         position="rewind", err=99)


!Lee los datos desde el archivo de la matriz

read(1, *) nfilas,ncolumnas

allocate(matriz (nfilas,ncolumnas) ,patron(nfilas,ncolumnas)) 
do i=1,nfilas
	do j=1,ncolumnas	!Hay un dato en cada línea
	read(1,*)valor	
	  if (valor == 0.0)	then
	  patron(i,j) = ""
	  else
      patron(i,j) = "x"
	  end if
	enddo
enddo


!	Imprime los datos en el archivo de patrón
do i=1,nfilas	
write(2,*) (patron(i,j),j=1,ncolumnas)!-----> Do implicito imprime un fila de una vez
enddo
	

close(1) ; close(2)
deallocate (matriz,patron)

stop 

99 stop "Se produjo un error al tratar de abrir este archivo" 
end program patron_sparse1
