
!PROGRAMA: media_arit_impar.f90
!CREADO POR: Erika Gomes 0032832

!Programa que cálcula la media aritmetica de los 
!primeros n numeros naturales impares 
!

program media_arit_impar

implicit none
!Declaracion
!N tendrá dos funciones en este programa, pues será una variable centinela
!y el número de elemenos impares que se usará para la media aritmetica
integer :: N, i,impar, media_aritmetica, suma 
!Se imprime en pantalla la descripción del programa
print '(//,16x,(a))', '********************************************'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*     Media Aritmetica de los primeros N   *'
print '(16x,(a))', '*         numeros naturales impares        *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*             ** OBSERVACI¢N **            *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*   Teclee un entero negativo para salir   *'
print '(16x,(a))', '********************************************'
!Se le pide al usuario un valor N positivo, al no ingresar un número positivo 
!se terminarà el progama
print '(//,5x,(a),\)', 'INTRODUZCA POR FAVOR UN VALOR ENTERO N= '
read *, N
do while (N>-1)!Siempre y cuando N no sea negativo se hará la suma
   suma=0; i=0 !Inicialización
   do while (i<=N-1)
      impar=2*i+1 !N impares
	  suma=suma+impar !Suma de los número impares
      i=i+1;
   end do
   if (n/=0) then
      media_aritmetica=suma/n !Siempre y cuando n sea diferente se cero, 
	                          !esta será la media aritmetica pedida
                              !Así se evita la división por cero   
   else
	  media_aritmetica=0      ! n=0, media aritmética es 0
   end if
   !IMPRESIÓN DE LOS RESULTADOS
   print '(//,16x,(a)  )', '********************************************'
   print '(16x,(a)     )', '*                                          *'
   print '(16x,(a)     )', '**               RESULTADOS:              **'
   print '(16x,(a)     )', '*                                          *'
   print '(16x,(a)     )', '*      Media Aritmetica de los primeros    *'
   print 1,N
   1 format(16x,'*',10x,i,20x,'*')
   print '(16x,(a)     )', '*         n£meros naturales impares        *'
   print '(16x,(a)     )', '*                    es                    *'
   print 2,media_aritmetica
   2 format(16x,'*',10x,i,20x,'*')
   print '(16x,(a)     )', '********************************************'

   !Se le pregunta al usuario si desea realizar otra operación, él colocará
   !un número negativo en caso de querer salir del programa y el valor de
   !N en caso de continuar con otra operación
   print '(//,15x,(a),\)', '******** DESEA REALIZAR OTRA OPERACI¢N??? ******** '
   print '(//,17x,(a),//)', '  ** Teclee un entero negativo para salir **'
   print '(//,5x,(a),\)', 'INTRODUZCA POR FAVOR UN VALOR ENTERO N= '
   read *, N
   end do
   print '(//)'
 end program media_arit_impar