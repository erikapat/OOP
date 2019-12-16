program patron_sparse

implicit none
real ::valor 
integer ::i,j,nfilas,ncolumnas
real, dimension(:), allocatable ::vec_fila
character(len=20), dimension(:), allocatable ::vec_patron 
character(len=20) ::arch_matriz, arch_patron
!ABRIR Y LEER ARCHIVOS
print*, "Nombre archivo matriz :"
read*,arch_matriz
open(unit=1, file=arch_matriz, status="old", action="read", & 
         position="rewind", err=99)

print*, "Nombre archivo patron :" 
read*, arch_patron
open(unit=2, file=arch_patron, status="unknown", action="write", & 
         position="rewind", err=99)

read (1, *) nfilas, ncolumnas

allocate (vec_fila(ncolumnas) , vec_patron(ncolumnas))

do i=1,nfilas
   read(1,*) vec_fila
   where (vec_fila == 0)
   vec_patron = ""
   elsewhere
   vec_patron = "x"
end where
write (2, *), vec_patron
enddo

close(1) ; close(2)
deallocate (vec_fila, vec_patron)
stop

99 stop "Se produjo un error al tratar de abrir este archivo" 
end program patron_sparse
