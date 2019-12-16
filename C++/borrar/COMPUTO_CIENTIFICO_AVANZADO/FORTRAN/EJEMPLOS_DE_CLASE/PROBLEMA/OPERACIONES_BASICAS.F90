
module operaciones_basicas

implicit none

type lista
  real :: valor
  integer::indice_columna
  type(lista), pointer :: sig
end type lista

type m_densa
  type(lista), dimension(:), pointer::fila
end type m_densa

end module operaciones_basicas