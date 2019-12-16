%Erika Gomes
%0032032
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                   %                                %
                                   %    PROGRAMA PRINCIPAL :        %
                                   %                                %
                                   %          'graf_exp'             %
                                   %                                %
                                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%grafica de x-tan(x) 
clf reset;
clf;
hold on;
%coordenadas en donde se ve más bonita la función
ezplot('0',[-7,7,-12,12]);
ezplot('1/x',[-7,7,-12,12]);
ezplot('exp(x)',[-7,7,-12,12]);
grid on;
hold off;

