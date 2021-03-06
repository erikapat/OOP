
!PROGRAMA: media_arit_impar.f90
!CREADO POR: Erika Gomes 0032832

!Programa que c�lcula la media aritmetica de los 
!primeros n numeros naturales impares 
!

program media_arit_impar

implicit none
!Declaracion
!N tendr� dos funciones en este programa, pues ser� una variable centinela
!y el n�mero de elemenos impares que se usar� para la media aritmetica
integer :: N, i,impar, media_aritmetica, suma 
!Se imprime en pantalla la descripci�n del programa
print '(//,16x,(a))', '********************************************'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*     Media Aritmetica de los primeros N   *'
print '(16x,(a))', '*         numeros naturales impares        *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*             ** OBSERVACI�N **            *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*                                          *'
print '(16x,(a))', '*   Teclee un entero negativo para salir   *'
print '(16x,(a))', '********************************************'
!Se le pide al usuario un valor N positivo, al no ingresar un n�mero positivo 
!se terminar� el progama
print '(//,5x,(a),\)', 'INTRODUZCA POR FAVOR UN VALOR ENTERO N= '
read *, N
do while (N>-1)!Siempre y cuando N no sea negativo se har� la suma
   suma=0; i=0 !Inicializaci�n
   do while (i<=N-1)
      impar=2*i+1 !N impares
	  suma=suma+impar !Suma de los n�mero impares
      i=i+1;
   end do
   if (n/=0) then
      media_aritmetica=suma/n !Siempre y cuando n sea diferente se cero, 
	                          !esta ser� la media aritmetica pedida
                              !As� se evita la divisi�n por cero   
   else
	  media_aritmetica=0      ! n=0, media aritm�tica es 0
   end if
   !IMPRESI�N DE LOS RESULTADOS
   print '(//,16x,(a)  )', '********************************************'
   print '(16x,(a)     )', '*                                          *'
   print '(16x,(a)     )', '**               RESULTADOS:              **'
   print '(16x,(a)     )', '*                                          *'
   print '(16x,(a)     )', '*      Media Aritmetica de los primeros    *'
   print 1,N
   1 format(16x,'*',10x,i,20x,'*')
   print '(16x,(a)     )', '*         n�meros naturales impares        *'
   print '(16x,(a)     )', '*                    es                    *'
   print 2,media_aritmetica
   2 format(16x,'*',10x,i,20x,'*')
   print '(16x,(a)     )', '********************************************'

   !Se le pregunta al usuario si desea realizar otra operaci�n, �l colocar�
   !un n�mero negativo en caso de querer salir del programa y el valor de
   !N en caso de continuar con otra operaci�n
   print '(//,15x,(a),\)', '******** DESEA REALIZAR OTRA OPERACI�N??? ******** '
   print '(//,17x,(a),//)', '  ** Teclee un entero negativo para salir **'
   print '(//,5x,(a),\)', 'INTRODUZCA POR FAVOR UN VALOR ENTERO N= '
   read *, N
   end do
   print '(//)'
 end program media_arit_impar