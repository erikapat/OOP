!
!PROGRAMA PRINCIPAL: coseno.f90
!CREADO POR: Erika Gomes, 0032832

!Programa que permite aproximar el valor cos(x), 
!para un valor x dado, mediante su serie de 
!Maclaurin

program coseno

implicit none
real::c
real(kind=8):: aprox_cos, term_serie, eps,x,fact,i,sum
!Se imprime en pantalla la descripción del programa
print '(//,16x,(a))', '********************************************'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*     Aproximar el valor cos (x) para un   *'
print '(16x,(a))', '*    valor x dado, mediante la serie de    *'
print '(16x,(a))', '*                 Maclaurin                *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '********************************************'
!Se le pide al usuario que introduzca el valor de X en radianes
print '(//,10x,(a),\)', 'INTRODUZCA POR FAVOR UN VALOR DE X (en radianes) = '
read *, x
!Se le pide al usuario que introduzca el valor de eps, el usado para las pruebas fue 1e-5
print '(/,10x,(a),\)','INTRODUZCA POR FAVOR EL VALOR DE EPSILON= ' 
read*, eps
!Imprime en pantalla los valores introducidos, a modo de verificación
print '(//,16x,(a))', 'LOS VALORES INTRODUCIDOS SON:'
print 1, x, eps
1 format(16x,'X = ',f8.3,10x'eps = ',e8.1)
!Inicialización:
aprox_cos=1.0 !sumatoria de los términos de la serie
term_serie=1.0 !término i de la serie
i=1.0 !i-esima iteración, al final de programa # de iteraciones
sum=0.0 !suma de dos iteraciones sucesivas, pieza clave a la hora de calcular el 
        !factorial del denominador de los término de la serie 
x=abs(x)!coseno es una función par, cos(x)=cos(-x)
!Cálculo de la serie hasta que el último término sea menor o igual a eps
do while(abs(term_serie)>eps)
   sum=i+(i-1)  !suma de dos iteraciones sucesivas, la actual con la anterior
   term_serie=-((x*x)*term_serie)/(2*i*sum)!en el caso de la segunda iteración en la cual
   !el denominador es cuatro factorial, este factorial se obtiene del denominador del término 
   !anterior=2 multiplicado por sum=2+1=3,llevamos 2*3,multiplicado por el denominador de 
   !la serie actual=4 asi tenemos 2*3*4=4!, sucede lo mismo para todos los término de la serie 
   aprox_cos=aprox_cos+term_serie !sumatoria de los términos de la serie, aprox de cos!!!!
   i=i+1.0 !iteración
end do 


!IMPRESIÓN DE LOS RESULTADOS
print '(//,16x,(a)  )', '********************************************'
print '(16x,(a)     )', '*                                          *'
print '(16x,(a)     )', '**               RESULTADOS:              **'
print '(16x,(a)     )', '*                                          *'
print 2,x
2 format(16x,'*',10x,'cos(',f8.3,')=',18x,'*')
print '(16x,(a),\)', '*                     '
print*,aprox_cos
print '(16x,(a)     )', '*                                          *'
print 3,i
3 format(16x,'*',10x,'ITERACIONES:',f10.1,10x,'*')
print '(16x,(a)     )', '*                                          *'   
print '(16x,(a)     )', '********************************************'

!Se imprime para poder comparar el resultado obtenido
c=cos(x)
print '(16x,(a)     )', '______________________________________________'
print '(16x,(a)     )', '______________________________________________'
print '(//,16x,(a),/)', 'COS(X) OBTENIDO CON LA FUNCI¢N cos de FORTRAN'
print '(/,16x,(a),\)',''!Esto es para que la solución se imprima con un margen de 16                         !En esta misma línea
print*,'cos(',x,')=',c
print '(16x,(a)     )', '______________________________________________'
read*
print '(//)'
end program coseno