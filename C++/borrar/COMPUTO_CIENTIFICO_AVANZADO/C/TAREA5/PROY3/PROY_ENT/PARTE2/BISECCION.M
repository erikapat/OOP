%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %           FUNCION :            %
                                   %                                %
                                   %             BISECCION          %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   
%METODO DE BISECCION
%Consiste en dividir consecutivamente los intervalos hasta encontrar el valor f(c)=0
function [x,error,iteracion]=biseccion(a,b,f,nmax,tol)
error=1;%----->error: no convergencia
iteracion=0;%inializacion de las variables de salida

x=0;
u=feval(f,a);%evaluación de la función en el punto
v=feval(f,b);
e=b-a;
if (sign(u)==sign(v))
   error=2;%------> error: signos iguales
   return
end
for k=1:nmax
   e=e/2;%división del intervalo entre [a,b]/2
   c=a+e;
   w=feval(f,c);
   if (abs(e)<tol | abs(w)<tol)
      error=0;%---------->  convergencia (no hay error)
      iteracion=k-1;
      x=c;
      return;
   end
   if (sign(w)~=sign(u))%con signos distintos se toman los valores:
      b=c;
      v=w;
   else%con signos iguales se toman los valores:
      a=c;
      u=w;
   end
end



