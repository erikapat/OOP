%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %    PROGRAMA PRINCIPAL :        %
                                   %                                %
                                   %                 pf             %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%PROGRAMA PRINCIPAL PARA LA RESOLUCION DE LA FRACCIÓN CONTINUA DE LA PARTE1 DEL PROYECTO 3
clc;
clear;
fprintf('\n\t\t\t\t   ***************************\n');
fprintf('\t\t\t\t    *** -   BIENVENIDO   - ***\n');
fprintf('\t\t\t\t   ***************************\n');
fprintf('\t\t\t\t____________________________________\n');
fprintf('\t\t\t\t|                                   |\n');
fprintf('\t\t\t\t| CALCULO DE LA FRACCION CONTINUA:  |\n');
fprintf('\t\t\t\t|                                   |\n');
fprintf('\t\t\t\t|           1                       |\n');
fprintf('\t\t\t\t|        ------------               |\n');
fprintf('\t\t\t\t|        P+  1                      |\n'); 
fprintf('\t\t\t\t|           ------                  |\n');
fprintf('\t\t\t\t|           P + 1                   |\n');
fprintf('\t\t\t\t|               ----                |\n');
fprintf('\t\t\t\t|                P+...              |\n');
fprintf('\t\t\t\t|                                   |\n');
fprintf('\t\t\t\t|                                   |\n');
fprintf('\t\t\t\t|MEDIANTE LA ITERACION DE PUNTO FIJO|\n');
fprintf('\t\t\t\t____________________________________\n\n\n');

fprintf('\t** -   INTRODUSCA LOS DATOS   - **\n');
%se verificará que los datos introducidos sean los correctos
%VALOR DE P
G=0;
while(G==0)
   fprintf(' \n\t\t');
   p=input('P= ');
   if (p<=1)  %los valores de p introducidos deben ser estrictamente mayores que 1    
      fprintf('\t\t\t\t* -   ERROR: p debe ser un número mayor a 1   - *\n');
      G=0;
   else if(p>=2000000) 
         fprintf('\t\t\t\t* -   ERROR: La solución para ese valor es cero   - *\n');
         G=0;
      else
         G=1; 
      end       
   end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%GRAFICA
clf;
whitebg;%cambia el color de la ventana a blanco, negro, blanco y así
%sucesivamente
hold on;
%de acuerdo con el valor de P se colocará la escala
%de x para que sea más fácil para el usuario ver la gráfica
if (p<=30)
   x=0:0.01:0.7;
else if (p>30 & p<=500) 
      x=0:0.01:0.05;
   else if (p>500 & p<5000) 
         x=0:0.001:3e-3;
      else if (p>=5000 & p< 200000)
            x=0:0.0001:3e-4;
         else
            x=0:0.00001:3e-5;
         end
      end
   end   
end
h=1./(p+x);
grid on;
plot(x,h);
plot(x,x), title('F(X) =1/(P+X) & F(X)=X');
shg; %----> si la gráfica ya esta abierta y minimizada esta función hace que se maximice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%APROXIMACION INICIAL   
fprintf('\n\tVEA LA GRÁFICA Y ESCRIBA UNA APROXIMACIÓN INICIAL!!!');
G=0;
while(G==0)% aproximación inicial: se pide una aproximacion inicial > 0
fprintf(' \n\t\t');
xo=input('Xo : Aproximacion Inicial =  ');
if (xo<=0 | xo>=1)
   fprintf('\t\t\t\t* -   ERROR: Escoja un Xo positivo, menor que 1 - *\n');
   G=0;
 else
      G=1;
   end  
end
%SE IMPRIMEN LOS RESULTADOS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc;
fprintf('\t\t\t\t_______________________________________________\n');
fprintf('\t\t\t\t|                                             |\n');
fprintf('\t\t\t\t| SOLUCION DE LA FRACCION CONTINUA:           |\n');
fprintf('\t\t\t\t|                                             |\n');
fprintf('\t\t\t\t|           1                                 |\n');
fprintf('\t\t\t\t|        ------------                         |\n');
fprintf('\t\t\t\t|        %.1f%s\n',p,' +  1                      '); 
fprintf('\t\t\t\t|                ---------                    |\n');
fprintf('\t\t\t\t|                %.1f%s\n',p,' + 1               ');
fprintf('\t\t\t\t|                      -------                |\n');
fprintf('\t\t\t\t|                       %.1f%s\n',p,' +...       ');
fprintf('\t\t\t\t|                                             |\n');
fprintf('\t\t\t\t|                                             |\n');
fprintf('\t\t\t\t|MEDIANTE LA ITERACION DE PUNTO FIJO          |\n');
fprintf('\t\t\t\t|_____________________________________________|\n\n\n');


%FUNCION
f=inline('(1/(p+x))');
%%%%%%%%%%%%%%%%%% PUNTO FIJO %%%%%%%%%%%%%%%%%%%
fprintf('\t__________________________\n');
fprintf('\n\t\t%s\n','PUNTO FIJO');
[x,iteracion,error]=puntofijo(f,p,xo);
if (error==1)
   fprintf('\n\tNo hubo convergencia\n');  
else
   fprintf('\n\t%s\n','SOLUCION:');
   fprintf('\n\t\t\t%f\n',x); 
   fprintf('\n\t%s\t%d\n','NUMERO DE ITERACIONES:',iteracion);
end 
fprintf('\t__________________________\n');


