
%grafica 
clf reset;
clf;
whitebg;
hold on;
%f=inline('x.^3+4.*x.^2-10');
%f=inline('x.^3-3*x-5');
%f=inline('2.*x.^3-x.^2+x-1');
%f=inline('x-sin(pi*x)');
%f=inline('exp(-x)');
%grafica para los valores de p>1
f1=inline('1./(1.000000000000000001+x)');
f2=inline('1./(2+x)');
f3=inline('1./(3+x)');
f4=inline('1./(4+x)');
x=0:0.001:1;
%y=f(x);
plot(x,f1(x),'-y'),title('GRAFICA'), xlabel('X'), ylabel('FUN');
plot(x,f2(x),'-r');
plot(x,f3(x),'-m');
plot(x,f4(x),'-g');


grid on;
plot(x,x,'-b'),legend('p = 1.1','p = 2','p = 3','p = 4','x = y');%x=y
plot(x,0,'-k');%x=0
%plot(0,y,'-r');
hold off;
%X^2+X*p-1