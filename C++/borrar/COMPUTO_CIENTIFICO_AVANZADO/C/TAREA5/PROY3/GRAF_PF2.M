%grafica 
clf reset;
clf;
hold on;
%f=inline('x.^3+4.*x.^2-10');
%f=inline('x.^3-3*x-5');
%f=inline('2.*x.^3-x.^2+x-1');
%f=inline('x-sin(pi*x)');
%f=inline('exp(-x)');
%grafica para los valores de p>1
f1=inline('x.^2+x*1.1-1');
f2=inline('x.^2+x*2-1');
f3=inline('x.^2+x*3-1');
f4=inline('x.^2+x*4-1');
x=0:0.001:1;
%y=f(x);
plot(x,f1(x),'-y'),title('GRAFICA'), xlabel('X'), ylabel('FUN');
plot(x,f2(x),'-r');
plot(x,f3(x),'-m');
plot(x,f4(x),'-g');


grid on;
plot(x,0,'-k'),legend('p = 1.1','p = 2','p = 3','p = 4','x = y');%x=y

%plot(0,y,'-r');
hold off;
%x^2+x*p-1