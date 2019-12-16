%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %          FUNCION :             %
                                   %                                %
                                   %          puntofijo             %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [x,iteracion,error]=puntofijo(f,p,xo)
x=0;
nmax=5000;      % nmax= numero máximo de iteraciones a realizar
tol=eps;        % tol= tolerancia, es decir, diferencia q se considera despreciable entre 
                % la solucioon real y la solucion obtenida
error=1;%----->no convergencia
iteracion=0;
v=feval(f,p,xo);% valo de la función para un p y un xo dado
for k=1:nmax 
   x1=v;
   v=feval(f,p,x1);
   if (abs(x1-xo)<tol*abs(x1)| abs(v)<tol)
      x=x1;
      error=0;%--------->convergencia
      iteracion=k-1;%número de iteraciones hechas
      return
   end
   xo=x1;
end


