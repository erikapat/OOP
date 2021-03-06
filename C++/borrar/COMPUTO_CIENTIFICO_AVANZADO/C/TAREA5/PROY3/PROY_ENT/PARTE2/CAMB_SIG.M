
%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %    PROGRAMA PRINCIPAL :        %
                                   %                                %
                                   %       cambio de signo          %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%cambio de signo
%Este algoritmo detecta el menor intervalo (aproximado),
%en el cual, por ejemplo la funci�n x-tan(x) cambia de signo.
clear;
clc;

%la idea es empezar a buscar el cambio de signo en el 
%extremo del intervalo m�s cercano a la soluci�n, para 
%lo cual es necesario ver la gr�fica antes de introducir los datos.

fprintf('\n\n\t\t** -   INTRODUSCA LOS DATOS   - **\n\n');
%SE TECLEA EL NOMBRE DE LA FUNCI�N COMO CADENA DE CARACTERES SIN COMILLAS
fprintf('\t\n\nTECLEE EL NOMBRE DE LA FUNCION(en minusculas,por favor):\n\t\t ');
name=input('FUNCION =','s');
f=inline(name);

fprintf('\n\t** -   ASEGURESE QUE LOS INTEVALOS SEAN CORRECTOS     - **\n');
fprintf('\t** -   USANDO UNA GR�FICA, PUES PODR�AN PRESENTARSE   - **\n');
fprintf('\t** -      RESPUESTAS POCO CONFIABLES O ILOGICAS       - **\n');
G=0;
while(G==0)
   fprintf(' \n\t');
   intervalo2=input('* Introdusca el extremo del intervalo m�s cercano a la soluci�n: ');
   fprintf(' \n\t');
   intervalo1=input('* Introdusca el otro extremo del intervalo: ');
   if (intervalo1==intervalo2)
      fprintf('\n\t\t\t    ***ERROR***\n');
      fprintf('\n\t\t\t***INTERVALOS IGUALES***\n');
      fprintf('\t:: Verifique nuevamente los valores ::\n\n');
      G=0;
   else
      G=1;
   end 
end

if (intervalo2>intervalo1)  
   delta=-0.01;%se revisara los valores del extremo derecho al izquierdo
else
   delta=0.01;%se revisara los valores del extremo izquierdo al derecho
end

x=intervalo2:delta:intervalo1;%vectro x, dependiendo del delta los valores del vector ser�n 
%crecientes o decrecientes
n=length(x);
for i=1:n-1
   h=x(i);%se revisaran los valores pr�ximos
   y=x(i+1);
   u=feval(f,h);%se evaluan los valores en la funci�n
   v=feval(f,y);
   if (u*v<0)% si la multiplicaci�n se hace menor que cero entonces 
             % hay un cambio de signo
      if (delta<0)%si se revisara los valores del extremo derecho al izquierdo
         a=y;% esta en la posici�n i+1 del vector x de valores decrecientes
         b=h;% esta en la posici�n i del vector x de valores decrecientes
      else%si se revisara los valores del extremo izquierdo al derecho
         a=h;% esta en la posici�n i del vector x de valores crecientes
         b=y;% esta en la posici�n i+1 del vector x de valores crecientes
      end
      clc;%se imprime los resultados
      fprintf('\n\t\t%s%s\n','FUNCION = ',name);
      fprintf('\n\t\t%s%f%s%f%s\n','En el intervalo [',a,',',b,' ] esta el cambio de signo.');
      return;
   end
end
clc;%en caso de no presentarse cambios de signo.
fprintf('\n\n\t\t%s%s\n','LA FUNCION = ',name);
fprintf('\n\t\tNO TIENE CAMBIO DE SIGNO EN EL INTERVALO DADO');


  