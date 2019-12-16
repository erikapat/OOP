fprintf('\nTECLEE EL NOMBRE DE LA FUNCION (encerrada en apóstrofos): ');
name=input('');
f=inline(name);
sol=feval(f,pi/4)

G=0;
while (G==0)
   name=input('FUNCION =');
   G=ischar(name)%VERIFICA QUE LO INTRODUCIDO SEAS CARACTERES
   if (G==0)
      fprintf('\n\t\t\t    ***ERROR***\n');
   end
end
