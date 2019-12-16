program prb

use mimod
 implicit none 
type(fila), pointer:: actual
call misub(5.1 ,principio) 
actual => principio

print*

print*,"En main:"

do while(associated(actual)) 
print* ,actual%valor
actual=>actual%sig
enddo

end program prb

