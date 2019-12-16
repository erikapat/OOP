
%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %           FUNCION :            %
                                   %                                %
                                   %                NEWTON          %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   
%METODO DE NEWTON
%La iteración del método de Newton es:
%X(n+1)=Xn f(Xn)/f'(Xn)
function [x,error,iteracion]=newton(xo,f,df,nmax,tol)
iteracion=0;%inializacion de las variables de salida
x=0;
error=1;%------>Error: no convergencia
%-------------->
v=feval(f,xo);%evaluación de la función en el punto
if(abs(v)<tol)
   error=0;%-------------------> no hay error: CONVERGENCIA
   x=xo;
   return;
end
for k=1:nmax
   deriv=feval(df,xo);%evaluacion de la derivada en el punto
   if (abs(deriv)<tol)
      %x=NaN -----------------> error: derivada nula
      error=2;
      return;
   end
   x1=xo-v/deriv;%X(n+1)=Xn f(Xn)/f'(Xn)
   v=feval(f,x1);%evaluación de la función en el punto X1
   if (abs(x1-xo)<tol*abs(x1)|abs(v)<tol)
      iteracion=k-1;
      error=0;%-------------------> no hay error: CONVERGENCIA
      x=x1;
      return;
   end
   xo=x1;   
end

