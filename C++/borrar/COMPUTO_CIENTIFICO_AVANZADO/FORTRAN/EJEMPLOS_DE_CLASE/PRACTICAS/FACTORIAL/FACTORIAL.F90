program factorial

implicit none
 
integer n

print *,"¿Entero al cual se le calculará el factorial? (negativo para terminar)" 
read *,n

do while (n>=0)
print *,"El factorial de ",n," es ",facto(n)
print *, "Entero al cual se le calculará el factorial? (negativo para terminar)"
read* ,n
enddo

contains

recursive function facto(n) result(fac)
implicit none

integer, intent(in):: n
integer fac 

if(n==0 .or. n==1)then
   fac=1
else 
   fac=n*facto(n-1)	
endif

end function facto

end program factorial
