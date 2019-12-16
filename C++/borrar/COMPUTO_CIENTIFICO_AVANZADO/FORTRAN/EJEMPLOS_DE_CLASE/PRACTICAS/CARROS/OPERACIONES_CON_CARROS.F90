module operaciones_con_carros

implicit none

public operator(==)
public assignment(=)
private:: compara_carros, asigna_carro

type carro
     real ::peso, longitud
     integer:: numeroID
end type carro

public carro

interface operator(==)
    module procedure compara_carros
end interface

interface assignment(=)
    module procedure asigna_carro
end interface


contains

function compara_carros(A,B) result(cc)
type(carro), intent(in) ::a,b
logical ::cc

if((a%peso == b%peso) .and. (a%longitud == b%longitud)) then 
   cc = .true.
else
   cc = .false.
end if
end function compara_carros


subroutine asigna_carro(a,b)
type(carro), intent(out):: a
type(carro), intent(in) :: b

 a%peso =b%peso 
 a%longitud = b%longitud
 a%numeroID = b%numeroID
end subroutine asigna_carro

end module operaciones_con_carros
