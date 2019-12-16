
                     /**************************************/
                    /*           GAUSS_SIMPLE.C          */
                   /*                                    */
                  /* Este programa resuelve un sist lin */
                 /*                Ax=b                */
                /*mediante eliminación gaussiana      */
               /*          simple sin pivoteo        */
              /**************************************/
                            
#include <stdio.h>  /*librería estándar*/
#include <stdlib.h>  /*asignacion dinámica de memoria*/
#include <conio.h>  /*getch();*/
#include <math.h>  /*LIBRERIA MATEMATICA*/
#define EPS 0.0000000001


void imprimir_matriz(int,double **);/*Imprime matriz por pantalla*/
void imprimir_vector(int,double *);/*imprime un vector por pantalla
se usará para imprimir a el vector del lado derecho b y el vector solución x*/
void error(int); /*mensajes de error*/
/*método de eliminación gaussiana simple, devuelve el vector sol y un entero
num_error (el error que pudiera presentarse en la función es la división por
cero, note que tenemos dos punteros constantes para impedir que esos valores
de entrada sean cambiados de alguna forma*/
void gauss_simple(int,double **const,double *const,int *, double *);
/*sustitución regresiva, la función se coloca dentro de la función gauss_simple*/
void sust_reg(int, double **, double *, double *);
/*verifica que la diagonal de la matriz introducida no tenga elementos iguales
a cero*/
int ver_diag(int,double **);

main()
{
 FILE *archivo;  /*puntero a nombre del archivo*/
 double **mat;  /*matriz nxn*/
 double *b;   /*vector del lado derecho del sistema Ax = b*/
 double *x;  /*soluciòn del sistema lineal*/
 char arch[25];  /*nombre del archivo*/
 /*Variables_
    n: orden de la matriz    i: índice       num_filas: numero de filas
    centinela: variable para detener el primer ciclo del p.p
    e: valor que indica que hay un error y se debe usar un continue para
    regresar al inicio del primer ciclo del p.p (ciclo con centinela)
    tamano: tamaño en bits de un double*/
 int n,i, num_error, num_filas,centinela=0,e, tamano=sizeof(double);
 /*c: se usará para leer matriz desde archivo, sin tener su orden
opc: opción escogida por el usuario, 1 para resolver sist lineales, 0 para salir*/
 char c,opc;


  do   /*inicio ciclo con centinela, centinela=0*/
  {
   
   n=0,num_error=0, num_filas=0, e=0; /*inicializacion*/

   clrscr(); /*limpia pantalla*/
   printf("\n");  /*MENU*/
   printf("\n\t\t**********************************************");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                ---  MENU  ---              *");
   printf("\n\t\t* OPERACIONES:                               *");
   printf("\n\t\t*       0.- SALIR                            *");
   printf("\n\t\t*       1.- RESOLVER UN SISTEMA LINEAL       *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t**********************************************");
   printf("\n"); /* ESPACIO;*/

   printf("\n\t\tINTRODUZCA OPCION:\t");
   scanf("%c",&opc); /*LECTURA */
   printf("%c\n", opc);

   switch(opc)  /*si el usuario teclea 0--> fin de programa y centinela pasa */
   {           /*de 0 a uno para asì finalizar el ciclo*/
    case '0':
             printf("\n\t\t*----- FIN DE PROGRAMA -----*");
             centinela=1; /*centinela vale ahora 1*/
             continue; /*se regresa al principio del ciclo sin ejecutar las sig*/
    default :          /*lineas, note que no volverà a entrar al ciclo*/
             if (opc!='1') /*valor diferente de 1,hece que se regrese al inicio
                             del ciclo, si se coloca el número -1, el compilador*/
              continue;     /*lo trasnformará a 1*/
             break;
   }
  /*_________________________APERTURA DEL ARCHIVO DE DATOS___________________*/
  printf("\n\t\tNOMBRE DEL ARCHIVO DE DATOS: ");
  scanf("%s",arch);
  if((archivo=fopen(arch,"r"))==NULL)
  {
   error(1);  /*función error, devolverá un mensaje de error, etiquetado con 1
               indicando que no se encuentra el archivo especificado*/
   continue; /*no se terminarà el programa, se mostrarà nuevamente el menu en
              caso de error*/
  }
 /*__________________________________________________________________*/

 /*___________________LEO MATRIZ DE DATOS DESDE ARCHIVO______________________*/
  /*se abre espacio para mat*/
  if((mat=(double**)malloc(sizeof(int)))==NULL)
  {
   error(2);  /*no se asigna memoria, memoria agotada*/
   continue;   /*no se termina el programa, se muestra nuevamente el menú*/
  }

  /*LECTURA DE DATOS*/

  if((mat[0]=(double*)malloc(tamano))==NULL)
  {
   error(2);   /*no se asigna memoria, memoria agotada*/
   continue;/*se vuelve al inicio del while centinela si ejecutar las sig líneas*/
  }
 /*_________________________PRIMERAS COLUMNAS________________________________*/
  do
  {
   if((fscanf(archivo,"%lf",mat[0]+n++))==EOF)
     if (n>1)     /*ya se leyo más de un elemento de la primera fila*/
     {            /*o no se encuentra el valor de salto de línea*/
      error(3);   /*ERROR:matriz a INCOMPLETA, FIN DE ARCHIVO*/
      e=1;    /*estamos dentro de un ciclo contenido en el ciclo centinela
      no se puede usar continue aquí para mostrar nuevamente el menú, se harà
      después de de salir de este ciclo, para eso es esta variable*/
      //free(mat);
      break;
     }
     else
     {
      error(6); /*el archivo esta completamente vació*/
      e=1;
      //free(mat);
      break;
     }

   fscanf(archivo,"%c",&c); /*leerá salto de línea o tabulación, hay que tener
   cuidado de teclear enter después de cada fin de fila de la matriz
   en el archivo*/
   if((mat[0]=(double*)realloc(mat[0], (n+1)*tamano))==NULL)
   {
    error(2); /*error de asignación de memoria*/
    e=1;
    break;
   }

  }while(c!='\n');  /*se ejecutará este ciclo siempre y cuando no se encuentre
  el caranter de fin de línea*/

  if (e) continue; /*si la variable e es diferente de cero se produjo error
  y  hay que devolverse al inicio del cilo centinela */
 /*______________________________________________________________________*/

  if((mat[0]=(double*)realloc(mat[0], n*tamano))==NULL)
  {
   error(2); /*error en asignación de memoria*/
   continue;
  }
  /*ya se leyo la primera línea, ahora que se conoce el orden de la matriz
  se procede a obtener las filas que faltan*/
  while(num_filas+1<n)
  {
   int i=0;
   ++num_filas;
   if((mat[num_filas]=(double*)malloc(n*tamano))==NULL)
   {
    error(2);      /*error en asignación de memoria*/
    e=1;
    break;
   }
    /*se leen los datos de la matriz a menos que ya no hallan datos y se
    encuentre END OF FILE antes de completar la matriz*/
   if((fscanf(archivo,"%lf",(mat[num_filas]+i++)))==EOF)
   {
    error(3);  /*matriz incompleta*/
    e=1;
    //free(mat);
    break;
   }

   for(i=1;i<n;i++)
      if((fscanf(archivo,"%lf",mat[num_filas]+i))==EOF)
      {
       error(3);
       e=2;                       /*MATRIZ INCOMPLETA*/
      // free(mat);
       break;
      }
   if (e==2) break;

  }
     if (e==1 || e==2)continue;  /*ERRORES  se vuelve al principio del while cent*/
  /*__________________________________________________________________*/

/*_____________________LEO VECTOR b DESDE ARCHIVO ___________________________*/
   /*se abre espacio para b, reservar espacio para el primer valor*/
  if((b=(double*)malloc(n*tamano))==NULL)
  {
   error(2);    /*error de asignación de memoria*/
   continue;
  }
  /*leo los valores de v que estan en una misma línea*/
  for(i=0;i<n;i++)
     if(((fscanf(archivo,"%lf",b+i))==EOF))
       if(i>0)
       {
        error(5);    /*b incompleto*/
        e=1;
        //free(mat);
        //free(b);
        break;
       }
       else
       {
        error(4);
        e=1;              /*no se encuentra b*/
        //free(mat);
        //free(b);
        break;
       }

   if (e) continue;            /*se regresa al inicio del ciclo centinela*/
/*_________________________________________________________________________*/

   fclose(archivo);  /*se cierra el archivo de datos*/
/*______________________solucion__________________________________________*/
   //clrscr(); /*limpia pantalla*/
   printf("\n");  /*S*/
   printf("\n\t\t**********************************************");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*      *------* SOLUCION *-------*           *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t**********************************************");
   printf("\n\n");

   imprimir_matriz(n,mat); /*imprime en pantalla la matriz leida por archivo*/
   printf("\n\t\tVECTOR b\n\n\t");
   imprimir_vector(n, b);    /*imprime por pantalla vector leído desde archivo*/

   /*asignación de mamoria para el vector solución*/
   if((x=(double*)malloc(n*tamano))==NULL)
   {
    error(2);/*error: no se asigna memoria*/
    continue;
   }

  gauss_simple(n,mat,b,&num_error,x);     /*método de gauss simple*/
  if (num_error==7)     /*error de divión por cero*/
  {
   error(num_error);   /*mensaje de error por pantalla*/
   /*free(x);
   free(mat);
   free(b); */
   continue;  /*inicio del ciclo centinela*/
  }
  else
  {
   printf("\n\t\tVECTOR SOLUCION\n\n\t");
   imprimir_vector(n,x);     /*imprime vector solución por pantalla*/
  }

  printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
  getch();/*especie de pausa*/

  /*free(x);
  free(mat);
  free(b); */

 }while(!centinela);        /*mientra la varible centinela sea igual a cero*/
 getch();   /*especie de pausa*/
}
/*__________________________FIN PROG PRINCIPAL__________________________*/

 /*imprime matriz por pantalla*/
 void imprimir_matriz(int n,double **mat)/*entra el orden de la matriz
 y la matriz A del sistema lineal Ax =b, aquí llamada mat*/
 {
  int i,j;                    /*imprime orden de la matriz*/
  printf("\n\t\tORDEN DE LA MATRIZ: %dx%d\n\n", n, n);
    printf("    ");
    for (i=0; i<n;i++)
    {
     for(j=0;j<n;j++)
       printf("%lf  ",mat[i][j]);     
    printf("\n");
    printf("    ");                                /*no devuelve nada (void)*/
    }
    printf("\n");
 }
 /*___________________________IMPRIMIR VECTOR______________________________*/
 void imprimir_vector(int n,double *b)
 {                             /*entra el orden de la matriz y el vector b*/
  int j;                       /*de longitud n*/
   for(j=0;j<n;j++)            /*no devuelve nada*/
    printf("%lf ",b[j]);
  printf("\n");

 }
 /*_______________________ERROR________________________________________*/
 void error(int e)
 {
  if (e==1)         /*errores que se pueden producir durante la ejecución */
  {                 /*del programa*/
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   NO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
  else if(e==2)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   MEMORIA NO ASIGNADA, FIN DE LA EJECUCION\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
  else if(e==3)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t    MATRIZ INCOMPLETA, FIN DE ARCHIVO\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
  else if(e==4)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t           VECTOR b NO ENCONTRADO\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
   else if(e==5)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   VECTOR b INCOMPLETO O MATRIZ A INCOMPLETA\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
   else if(e==6)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t              ARCHIVO VACIO!!!\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }
   else if(e==7)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   DIVISION POR CERO, FIN DEL CALCULO!!!\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
   //exit(1);
  }                             /*método de gauss simple sin pivoteo*/
 }
 /*_______________________________GAUSS_SIMPLE______________________________*/
 void gauss_simple(int n,double **const mat,double *const b,int *num_error,double *sol)
 {                 /*entra: el orden de la matriz, la matriz A, el vector b*/
  int k,i,j;      /*sale: un entero con el número de error cometido*/
  double xmult, **mat1,*b1;   /*y el vector solución sol*/

  /*INICIALIZACIÓN*/
  mat1=mat; /*asignación*/
   b1=b;
  for (k=0;k<n-1; k++)sol[k]=0;

  *num_error=ver_diag(n,mat1);   /*verificar que las diagonales de la matriz  */
  if (!*num_error)          /*no sean iguales a cero*/
  {                         /*si no hay riesgo de división por cero se prodece*/
   for (k=0;k<n-1; k++)             /*a la resolución*/
    for (i=k+1;i<n;i++)
    {

     xmult=mat1[i][k]/mat1[k][k];      /*se modifica aquí la matriz, ojo: esta */
     mat1[i][k]=xmult;                /*modificación no saldra de esta función*/
     for(j=k+1;j<n;j++)
       mat1[i][j]-=xmult*mat1[k][j];

     b1[i]-=xmult*b1[k];         /*se modifica b*/
    }

      sust_reg(n,mat1,b1,sol); /*función de sust regresiva*/
   }
 }
   /*__________________________________________________________________*/
   int ver_diag(int n,double **mat)
   {
    int k,num_error=0; /*si los elem diagonal tien al menos un valor igual
    a cero, se producira u error*/

    for (k=0;k<n; k++)
    {
     if (fabs(mat[k][k])<EPS)/*si valor abs de el elem de la matriz menor a eps*/
     {       /*mensaje de error etiquetado con el número siete*/
        num_error=7;
        break;         /*si no es cero el valor de num_error será cero*/
     }
    }
    return(num_error);/*devuelve el entero num_error*/
   }
      /**/
  /*_______________________SUST. REGRESIVA_______________________*/

  void sust_reg(int n, double **mat, double *b, double *sol)
  {                  /*sust regresiva, entra matriz modificada mat1, vector*/
   double suma;      /*modificado b1 y devuelve la solución del sist lineal*/
   int i, j;

   sol[n-1]=b[n-1]/mat[n-1][n-1]; /*se empieza a hallar los valores del vector*/
   for (i=n-2; i>=0;i--) /*sol desde la última fila de la matriz y el */
   {                    /*último valor de el vectro b*/
    suma=b[i];
    for (j=i+1;j<n;j++)
     suma-=mat[i][j]*sol[j];   /*despeje y solución*/
     sol[i]=suma/mat[i][i];
   }
  }
  /*____________________________________________________________*/







