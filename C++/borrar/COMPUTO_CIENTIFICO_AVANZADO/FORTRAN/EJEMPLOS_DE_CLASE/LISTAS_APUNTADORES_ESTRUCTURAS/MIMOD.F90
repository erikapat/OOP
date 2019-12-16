
module mimod

implicit none

type fila
real :: valor
type(fila), pointer:: sig
end type fila

type(fila), pointer :: principio 
contains

function mif(val)result(p) 
real, intent(in) ::val
type(fila), pointer :: p,q

allocate(p)
p%valor =val
allocate(q)
q%valor = val + 2
p%sig=>q
nullify(q%sig)
print*,"En mif:" 
print* ,p%valor 
print*,q%valor
print* 
end function mif

subroutine misub(val,p) 
real, intent(in) ::val
 type(fila), pointer ::p,q

allocate(p)
p%valor = val
allocate(q)
q%valor =val + 2
p%sig=>q
nullify(q%sig)

print*, 'En misub'
print*,p%valor 
print*,q%valor
print*
end subroutine misub

end module mimod
