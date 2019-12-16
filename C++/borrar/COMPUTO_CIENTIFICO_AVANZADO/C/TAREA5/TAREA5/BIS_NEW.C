

#include <stdio.h>/*BIBLIOTECA ESTANDAR*/
#include <string.h>/*BIBLIOTECA DE GETCHAR*/
#include <math.h> /*funciones matematicas (abs, cos,...)*/
#include <conio.h>/*clrscr(); /*limpia pantalla*/

double f1(double);/*FUNCIÓN x=tan(x)*/
double deriv_f1(double); /*DERIVADA DE LA FUNCIÓN ANTERIOR*/
double f2(double);/*FUNCIÓN x=cos(x/2)*/
double deriv_f2(double); /*DERIVADA DE LA FUNCIÓN ANTERIOR*/
double f3(double);/*FUNCIÓN 1/x=exp(x)*/
double deriv_f3(double); /*DERIVADA DE LA FUNCIÓN ANTERIOR*/
double bis(double (*f)(double x1),double, double, double);/*FUNCIÓN PARA
                                                         EL PASO DE BISECCION*/
int num_max_iter(void); /*introducir por teclado número de iteraciones y ver si el
                          número introducido es correcto*/
double tolerancia(void); /*introducir por teclado la tolerancia*/
void intervalo(double*, double*);  /*introducir intervalo [a,b]*/
/*double derivada(double); /*calcula la derivada de una función*/
double newton(double (*f)(double x1),double (*deriv_f)(double x1),double, double); /*paso de newton*/
/*método combinado de newton-biseccion*/
void bis_new(double (*f)(double x1),double (*deriv_f)(double x1),double, double, double,int,double*,int*,int*);
 /************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!PROGRAMA PRINCIPAL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 main(void)
 {
  int nmax, iter=0, logico=0;
  double tol, a=0.0,b=0.0, x1=0.0;
  char opc;

  do
  {
   clrscr(); /*limpia pantalla*/
   printf("\n");  /*MENU*/
   printf("\n\t\t**********************************************");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                ---  MENU  ---              *");
   printf("\n\t\t* OPERACIONES:                               *");
   printf("\n\t\t*       0.- SALIR                            *");
   printf("\n\t\t*       1.- FUNCION 1: X=TAN(X)              *");
   printf("\n\t\t*       2.- FUNCION 2: X=COS(X/2)              *");
   printf("\n\t\t*       3.- FUNCION 3: 1/X=EXP(X)            *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t**********************************************");
   printf("\n"); /* ESPACIO;*/

   printf("\n\t\tINTRODUZCA OPCION: ");
   scanf("%c",&opc); /*LECTURA */
   /*opc=getchar();*/
   if(opc=='0')
   {
    printf("\n\n\t\tFIN DEL PROGRAMA\n");
    break;
   }
   else if(opc!='1' && opc!='2' && opc!='3')
   continue;
 
   /*pido datos*/
   printf("\n\t\t\t ***************************\n\n");
   printf("\n\t\t\t ** DATOS DE ENTRADA **\n\n");
   nmax=num_max_iter();   /*NUMERO MAX DE ITERACIONES*/
   tol=tolerancia();      /*TOLERANCIA*/
   intervalo(&a,&b);        /*[A,B]*/

   clrscr(); /*limpia pantalla*/
   printf("\n\t\t\t ***************************\n\n");
   printf("\n\t\t\t     ** RESULTADOS **\n\n");
   printf("\n\t\t\t ***************************\n\n");

   switch(opc)
   {
    case '1':
            bis_new(f1,deriv_f1,a,b,tol,nmax,&x1,&iter,&logico);

            /****************FUNCION 1: X=TAN(X)*********************/
            if(logico)
            printf("\n\n\t\tNO HUBO CONVERGENCIA\n"); /*NO CONVERGE*/
            else
            {
             printf("\n\n\t\t------ ** X=TAN(X) ** -------"); /*convergencia*/
             printf("\n\n\t\tX1: %.16lf",x1);/*imprime 16 digitos
                                               en la parte decimal*/
             printf("\n\n\t\titeraciones: %d\n\n\n",iter);
            }
            printf("\n\n\t\tPRESIONE ENTER PARA CONTINUAR");
            getch(); //ESPECIE DE PAUSA
            break;
   case '2':
            /****************FUNCION 2: X=COS(X/2)*********************/
            bis_new(f2,deriv_f2,a,b,tol,nmax,&x1,&iter,&logico);

            if(logico)
            printf("\n\n\t\tNO HUBO CONVERGENCIA\n"); /*NO CONVERGE*/
            else
            {
             printf("\n\n\t\t------ ** X=COS(X/2) ** -------");/*convergencia*/
             printf("\n\n\t\tX1: %.16lf",x1);/*imprime 16 digitos en la parte decimal*/
             printf("\n\n\t\titeraciones: %d\n\n\n",iter);
             printf("\n\n\t\tPRESIONE ENTER PARA CONTINUAR");
             getch(); //ESPECIE DE PAUSA
            }
            break;
   case '3':
           /****************FUNCION 3: 1/X=EXP(X)*********************/
           bis_new(f3,deriv_f3,a,b,tol,nmax,&x1,&iter,&logico);
           printf("\n\t\t\t ***************************\n\n");
           if(logico)
           printf("\n\n\t\tNO HUBO CONVERGENCIA\n"); /*NO CONVERGE*/
           else
           {
            printf("\n\n\t\t------ ** 1/X=EXP(X) ** -------"); /*convergencia*/
            printf("\n\n\t\tX1: %.16lf",x1);/*imprime 16 digitos en la parte decimal*/
            printf("\n\n\t\titeraciones: %d\n\n\n",iter);
            printf("\n\n\t\tPRESIONE ENTER PARA CONTINUAR");
            getch(); //ESPECIE DE PAUSA
           }
           break;
          
  }
 } while(opc!='0');
  getch(); //ESPECIE DE PAUSA
  
 }
/**************************FIN DE PROGRAMA PRINCIPAL *************************/

   /************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double f1(double x)    /*recuerda que los valores son en radianes*/
 {
  return(x-tan(x)); /*función a trabajar*/

 }
/************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double deriv_f1(double x)    /*recuerda que los valores son en radianes*/
 {
  return(-pow(tan(x),2)); /*derivada de la función anterior*/
  /*FORMA SIMPLIFICADA -TAN(X)^2*/
 }
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double f2(double x)    /*recuerda que los valores son en radianes*/
 {
  return(x-cos(x/2)); /*función a trabajar */

 }
/************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double deriv_f2(double x)    /*recuerda que los valores son en radianes*/
 {
  return(1+(1/2)*sin(x/2)); /*derivada de la función anterior*/

 }
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double f3(double x)    /*recuerda que los valores son en radianes*/
 {
  return(1/x-exp(x)); /*función a trabajar */

 }
/************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN F1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double deriv_f3(double x)    /*recuerda que los valores son en radianes*/
 {
  return(-1/(x*x)-exp(x)); /*derivada de la función anterior*/
  
 }/*
 /************************************************************************/
 /*!!!!!!!!!!FUNCION PARA PEDIR POR TECLADO NUM MAX DE ITERACIONES!!!!!!!!!!!*/
 int num_max_iter(void)
 {
  int G=0, nmax;

  while(G==0)
  {
   printf("\n\t\tINTRODUZCA NUMERO MAXIMO DE ITERACIONES: ");
   scanf("%d", &nmax);/*valor introducido por teclado*/
   if (nmax<=0) /*no se permiten entero negativos ni iguales a cero*/
   {
    clrscr(); /*limpia pantalla*/
    printf("\n           _________________________________________________");
    printf("\n\t\t\t  ***  ERROR  ***\n");
    printf("\n\t\t\t***NMAX NEGATIVO O CERO***");
    printf("\n\t\t    :: Verifique nuevamente el valor::\n");
    printf("\n          __________________________________________________\n\n");
    G=0;
   }
   else if(nmax>50000)  /*número muy grande de iteraciones, se acepta pero
   se le advierte al usuario*/
   {
    printf("\n\t\t\t***NMAX MUY GRANDE***");
    G=1;
   }
   else
    G=1;
  }  /*fin de while*/
  return(nmax);
 } /*FIN DE FUNCIÓN NUM_MAX_ITER*/
 /************************************************************************/
 /*!!!!!!!!!!FUNCION PARA PEDIR POR TECLADO LA TOLERANCIA!!!!1!!!!!!!!!!!*/
  double tolerancia(void)
  {
   int G=0;
   double tol;
   while (G==0)
   {
    G=1;
    printf("\n\t\tINTRODUZCA LA TOLERANCIA: ");
    scanf("%lf",&tol);

    if (tol<=0 || tol>=1)
    {                       /*ERROR: TOLERACIA MAYOR QUE 1*/
     clrscr(); /*limpia pantalla*/
     printf("\n           _________________________________________________");
     printf("\n\t\t\t  ***  ERROR  ***\n");
     printf("\n\t\t    :: Verifique nuevamente el valor::\n");
     printf("\n          __________________________________________________\n\n");
     G=0;
    }

   } /*FIN WHILE*/
   return(tol);
  }/*fin función tolerancia*/
 /************************************************************************/
 /*!!!!!!!!!!FUNCION PARA PEDIR POR TECLADO INTERVALO [A,B]!!!!!!!!!!!!!!!*/
  void intervalo(double *a, double *b)
  {
   int G=0;     /*SE USA PASO DE PARAMETRO POR REFERENCIA PARA PODER TENER
                 DOS PARAMETROS DE SALIDA*/
   double aa,bb;
   printf("\n\t\t\tINTERVALO [a, b]");
   printf("\n\t\t::donde se produce el cambio de signo de la funcion::");
   while (G==0)
   {
    G=1;
    printf("\n\t\tINTRODUZCA a= ");
    scanf("%lf",&aa);
    *a=aa;    /*SE LEEE Y DESPUÈS SE ASIGNA */
    printf("\n\t\tINTRODUZCA b= ");
    scanf("%lf",&bb);
    *b=bb;
    if (*a==*b|| *a>*b || *a==0 || *b==0)  /*NO DEBE SUCEDER*/
    {
     printf("\n\t\t\t  *** ERROR ***\n");
     printf("\n\t:: Verifique nuevamente el valor::\n\n");
     G=0;
    }

   } /*FIN WHILE*/
  }  /*END FUNCION intervalo*/
   /************************************************************************/
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN BISECCION!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double bis(double (*f)(double x1),double xo, double a, double b)/*FUNCIÓN BISECCION*/
 {
  double fxo,fa,fb;

  fxo=f(xo);
  fa=f(a);
  fb=f(b);

  if(fa*fb>=0)  /*SIGNOS IGUALES: ambos positivos o ambos negativos*/
  {
   printf("\n\t\tERROR: Signos Iguales\n");
   return(0);
  }

  if(fxo*fa<0)
     b=xo;
  else
     a=xo;

  return((a+b)/2);
}/*FIN FUNCIÓN BISECCIÓN*/
/************************************************************************/
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN DERIVADA!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 /*función que calcula la derivada númerica a travès de una aprox lineal*/
/* double derivada(double (*f)(double x1),double x) /*punto donde se evalua la derivada*/
 /*{
  double h=0.001, der;  /**/
 /* der=(f(x+h)-f(x-h))/(2*h); /*aprox lineal*/
 /* return(der);
 }  */
 /************************************************************************/
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN newton!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 double newton(double (*f)(double x1),double (*deriv_f)(double x1),double xo, double tol)
 {
  double fxo, x,dfxo;

  fxo=f(xo);
 /*abs(fx1)<=tol*/
  if (sqrt(fxo*fxo)<=tol) /*convergencia*/
  {
   x=xo;
   return(x);
  }
 /*derivada de la función en el punto*/
  dfxo=deriv_f(xo);
  x=xo-(fxo/dfxo);  /* X=Xo-(f(Xo)/f'(Xo))*/

  return(x);
 }/*FIN NEWTON*/
  /************************************************************************/
 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!FUNCIÓN bis_new!!!!!!!!!!!!!!!!!!!!!!!!!!*/
 void bis_new(double (*f)(double x1),double (*deriv_f)(double x1),double a, double b, double tol,int nmax,double *x,int *iter, int *logico)
 {
  int iteracion=1;
  double xo, dfxo,x1,fx1;
  *logico=0;/*inicializacion*/

  xo=(a+b)/2;

  while(iteracion<=nmax)
  {
     /*derivada de la función en el punto*/
  dfxo=deriv_f(xo);
   if (dfxo==0)
   {
    /*bisección*/
    xo=bis(f,xo,a,b);
    iteracion++;
    continue; /*salta hasta el principio del bucle, dejando sin ejecutar las
               líneas restantes depuès del continue*/
   }
   x1=newton(f,deriv_f,xo,tol); /*llamada a la función newton*/
   if(x1<a || x1>b)/**/
   {
    xo=bis(f,x1,a,b);
    iteracion++;
    continue;
   }
   fx1=f(x1);/*evalua la función en x1*/
   if(sqrt(fx1*fx1)<=tol)/*abs(fx1)<=tol, la función abs de C es solo
                           para enteros*/
   {
     *x=x1;
     *iter=iteracion;
     break;
   }
   xo=x1;   /*esta funcion devuelve x1 */
   iteracion++;
  } /*FIN WHILE*/
  if(nmax==iteracion)
  {
   *x=0;
   *logico=1;
   *iter=nmax;
  }

 }/*FIN FUNCIÓN BIS_NEW*/
/************************************************************************/
