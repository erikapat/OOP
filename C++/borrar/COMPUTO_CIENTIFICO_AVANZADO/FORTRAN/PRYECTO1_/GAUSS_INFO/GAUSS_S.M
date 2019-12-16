function [x,iter]=gauss_s(A,b,xo,tol);
  % Método iterativo de Gauss-Seidel
  % function [x,iter]=gauss_s(A,b,xo,tol):

  iter=1;
  D=diag(diag(A));   % Descomposición A=D+L+U
  L=tril(A)-D;
  U=triu(A)-D;
  
  x=xo;                 % | Inicialización 
  xk=(D+L)\(-U*x+b);    % |
  if norm(x)==0,
    aux=norm(xk-x);
  else 
    aux=norm(xk-x)/norm(x);
  end;
 
  while aux>tol & iter<500,
   iter=iter+1;
   x=xk;
   xk=(D+L)\(-U*x+b);
   aux=norm(xk-x)/norm(x);
  end;
  
  if iter>=500,
    disp('El método no converge en el máx. de iteraciones');
    disp(iter);
    x=Inf;
  else
    x=xk;
  end;


  