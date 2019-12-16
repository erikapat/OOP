%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %    PROGRAMA PRINCIPAL :        %
                                   %                                %
                                   %          'graf_cos'             %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%grafica de x-tan(x) 
clf reset;
clf;
hold on;
%coordenadas en donde se ve más bonita la función
ezplot('0',[-6,14,-1.2,1.2]);
ezplot('x',[-6,14,-1.2,1.2]);
ezplot('cos(x/2)',[-6,14,-1.2,1.2]);
grid on;
hold off;

