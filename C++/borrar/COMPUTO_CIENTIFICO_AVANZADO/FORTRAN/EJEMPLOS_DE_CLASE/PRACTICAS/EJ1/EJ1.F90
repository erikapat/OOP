program ejl

implicit none
real a,b,c

a=1.0
b=2.0
c=3.0

call misub(a,b,c)
end program ejl



subroutine misub(a,b,c)

implicit none
real, intent(in) :: a,b,c 
real w

w=a+b+c

call misub2

contains

subroutine misub2
print *,"En misub2: w=", w
end subroutine misub2

end subroutine misub
