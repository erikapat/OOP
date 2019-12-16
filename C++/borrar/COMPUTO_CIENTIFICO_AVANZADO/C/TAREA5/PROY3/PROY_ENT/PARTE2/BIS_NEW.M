%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %    PROGRAMA PRINCIPAL :        %
                                   %                                %
                                   %          'bis_new'             %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear;
clc;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%DATOS**********************************************
%SE TECLEA EL NOMBRE DE LA FUNCIÓN COMO CADENA DE CARACTERES SIN COMILLAS
fprintf('\n\n\t\t** -   INTRODUSCA LOS DATOS   - **\n');
fprintf('\t\t** ---------------------------- **');
%funciones
fprintf('\n\t\t** -TENGA CUIDADO AL INTRODUCIR- **\n');
fprintf('\t\t** -     LOS DATOS POR FAVOR   - **\n\n');

 
fprintf('\t\n\nTECLEE EL NOMBRE DE LA FUNCION (en minusculas, por favor):\n\t\t ');
name=input('FUNCION =','s');
f=inline(name);
d=diff(name); %----------------->OJO: ES NECESARIO TENER INSTALADO: -------> FUNCIÓN PARA DERIVAR
              % EL 'EXTENDED SIMBOLIC MATH TOOLBOX' DE MATLAB
fprintf('\n\n\t\tDERIVADA DE LA FUNCION INTRODUCIDA= ');
disp(d);              
fprintf('\n\t\t\t*** SI ESTA DEACUERDO INTRODUZCALA***\n');
fprintf('\n\t\t');
derivada=input('DERIVADA =','s');
df=inline(derivada);                                     

 G=0; 
while(G==0)
   fprintf('\n\t\t');
   nmax=input('Introduzca numero máximo de iteraciones = ')+1; 
   if (nmax<=0)
      fprintf('\n\t\t\t    ***ERROR***\n');
      fprintf('\n\t\t\t***NMAX NEGATIVO***\n');
      fprintf('\t:: Verifique nuevamente el valor ::\n\n'); 
      G=0;
   else if(nmax>=50000)
         fprintf('\n\t\t\t***NMAX MUY GRANDE***\n');
         fprintf('\t:: EL CÁLCULO REQUERIRA DE ALGUNOS SEGUNDOS ::\n\n');
         G=1;
      else
         G=1;
      end
   end
end
  G=0; 
   while(G==0)
      fprintf('\t\t');
      tol=input('Introduzca tolerancia = '); 
      if (tol<eps | tol>1)
         fprintf('\n\t\t\t***ERROR***\n');
         fprintf('\t:: Verifique nuevamente el valor ::\n\n'); 
         G=0;
      else
         G=1;
      end
   end

G=0; 
fprintf('\n\t\t\t*** INTERVALO DE APROXIMACIÓN PARA EL METODO DE BISECCION***\n');
while(G==0)
   G=1;
   fprintf('\n\t\t');
   a=input('a= '); 
   fprintf('\n\t\t');
   b=input('b= ');
   if (a==b | a>b | a==0 | b==0)
      G=0;
      fprintf('\n\t\t\t***ERROR***\n');
      fprintf('\t:: Verifique nuevamente los valores ::\n\n');
   end
end
fprintf('\n\t\t\t*** INTERVALO DE APROXIMACIÓN PARA EL METODO DE NEWTON***\n');
G=0; 
while(G==0)
   G=1;
   fprintf('\n\t\t');
   xo=input('Xo= '); 
   if (xo<=0)
      G=0;
      fprintf('\n\t\t\t***ERROR***\n');
      fprintf('\t:: Verifique nuevamente el valor de Xo ::\n\n');
   end
end


%METODO BISECCION
clc;
%%%%%%%%%%%%%%%%%% BISECCION %%%%%%%%%%%%%%%%%%%%
fprintf('\n\n\t\t%s%s\n','LA FUNCION = ',name);
fprintf('\t__________________________\n');
fprintf('\n\t\t%s\n','METODO BISECCION');
[x,error,iteracion]=biseccion(a,b,f,nmax,tol);%FUNCION BISECCION
if (error==1)
   fprintf('\n\tNo hubo convergencia\n');  
else if(error==2) 
      fprintf('\n\tERROR: signos iguales \n');
   else
      fprintf('\n\t%s\n','SOLUCION:');
      fprintf('\n\t\t\t%f\n',x); 
      fprintf('\n\t%s\t%d\n','NUMERO DE ITERACIONES:',iteracion);
   end 
end

%%%%%%%%%%%%%%%%%% NEWTON - RAPSON %%%%%%%%%%%%%%%%%%%
fprintf('\t__________________________\n');
fprintf('\n\t\t%s\n','NEWTON - RAPSON');
[x,error,iteracion]=newton(xo,f,df,nmax,tol);%FUNCIÓN NEWTON 
if (error==1)
   fprintf('\n\tNo hubo convergencia\n');  
else if(error==2) 
      fprintf('\n\tERROR: Derivada nula \n');
   else
      fprintf('\n\t%s\n','SOLUCION:');
      fprintf('\n\t\t\t%f\n',x); 
      fprintf('\n\t%s\t%d\n','NUMERO DE ITERACIONES:',iteracion);
   end 
end
fprintf('\t__________________________\n');
