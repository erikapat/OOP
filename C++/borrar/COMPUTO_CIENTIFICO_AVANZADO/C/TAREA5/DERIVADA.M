%programa que calcula la derivida numérica a traves de una
%aproximación lineal.....
%Primero defines tu función normal
%en este caso usaremos F(x)=sin(x)
H=0.001
%x es el punto donde deseas evaluar la derivada...
x=5

F=inline('sin(x)','x')

%definimos ahora, la funcion derivada...de F
der=(F(x+H)-F(x-H))/(2*H)
% La funci'on der es la aproximaci'on lineal, esta es la que me se
% de memoria existen aproximaciones de orden dos, tres y cuatro
%que disminuyen el error notablemente, aunque de esta manera de buenos resultados
%date cuanta que el H debe ser pequeno pero no mucho por que si lo hace
%extremadamente pequeno no da respuewsta

%notar que en este caso p[odemos concocer la derivada
%exacta que es dF=cos(x), DE ESTA MANERA observamos
%que tan buena es nuestra aproximacion

dF=inline('cos(x)','x')

derExacta=dF(x)
% para de esta manera saber que tan buena es nuestra aprox
error=der-derExacta