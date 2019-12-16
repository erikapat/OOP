

#include <stdio.h> /*librería estándar*/
#include <stdlib.h> /*librería para exit()*/
#include <conio.h>  /*getch();*/
#include <math.h> /*librería para pow*/

#define EPSILON 0.0000000001
#define FALSE 0
#define TRUE  1
/*____________________________ESTRUCTURAS________________________________________*/

/*______________ESTRUCTUA PARA LA LISTA DE VERTICES: LLAMADA VERTICES_________*/
typedef struct elem
{
 int vertice;  /*NUMERO QUE IDENTIFICA AL VERTICE*/
 double *coordenadas; /*COORDENADAS DEL VERTICE, VECTOR DE TAMAÑO 2 [X, Y]*/
 struct elem *siguiente;
}vertices;
/*__________ESTRUCTURA PARA LA LISTA DE TRIANGULOS: LLAMADA TRIANGULOS____________*/
typedef struct casilla
{
 int triangulo; /*NUMERO QUE IDENTIFICA A TRIANGULO*/
 int *num_vertice;/*VECTOR VERTICES DE CADA TRIANGULO DE DIMENSIÓN 3 [V1 V2 V3]*/
 struct casilla *siguiente;
}triangulos;
/*____ESTRUCTURAS PARA LA LISTA DE TRINGULOS ASOCIADOS A CADA VERTICE_________*/
/*LA IDEA ES GUARDAR EN UNA MATRIZ ESTOS VALORES, DONDE CADA FILA ES LA LISTA
DE TRIANGULOS PARA UN DETERMINADO VERTICE*/
/*LISTAS(FILAS DE LA MATRIZ)*/
typedef struct list
{
 int vert, triang;  /*LA LISTA CONTIENE LOS VALORES DEL VERTICE Y EL TRIANGULO,
                    NOTE QUE EL VALOR DEL VERTICE EN TODA LA LISTA SE´RA EL MISMO */
 struct list *siguiente;
}triang_asociado_a_vert; /*NOMBRE DE LA ESTRUCTURA*/
/*MATRIZ QUE TIENE COMO FILAS LISTAS*/
typedef struct mat
{
 triang_asociado_a_vert *comienzo;  /*APUNTADOR A COMIENZO DE LA LISTA*/
 struct mat *siguiente;
}matriz; /*NOMBRE DE LA ESTRUCTURA*/
/*________________LISTAS DE TRIANGULOS VECINOS ____________________________*/
/*IDEA PARECIDA A LA DEL CASO ANTERIOR, PARA CADA TRIANGULO HABRÁ UN LISTA QUE
CONTEDRA A SUS TRIANGULOS VECINOS, EL CONJUNTO DE VARIAS DE ESAS LISTA ESTARÁ
EN OTRA ESTRUCTURA PARECIDA A UNA MATRIZ*/
typedef struct VECINOS
{
 int triang, triang_vecinos; /*LA LISTA CONTENDRA A TRIANGULO Y SUS VECINOS*/
 struct VECINOS *siguiente;
}vecinos;  /*IDENTIFICADOR DE LA ESTRUCTURA*/

typedef struct matriz_de_vecinos
{
 vecinos *comienzo;    /*APUNTADO A COMIENZO DE LA LISTA (COMIENZO DE CADA FILA)*/
 struct matriz_de_vecinos *siguiente;
}mat_vec;   /*IDENTIFICADOR DE LA ESTRUCTURA*/
/*_________________LISTA CON VALORES DE INDICE DE CALIDAD____________________*/
typedef struct i_c
{
 int triangulo; /*triangulo al cual se calcula el indice de calidad*/
 double indice_calidad; /*valor del indice de calidad*/
 struct i_c *siguiente;
} list_i_c;  /*NOMBRE DE LA ESTRUCTURA*/
/*_____________________________________________________________________*/

/*PROTOTIPOS*/
vertices *crear_lista_de_vertices(char[], int*, int *); /*LISTA QUE CONTIENE LOS VERTICES DE LOS
                                            TRIANGULOS Y SUS COORDENADAS*/
void imprimir_list_vertices(vertices *);  /*IMPRIME LISTA DE VERTICES CON SUS COORDENADAS*/
void verificar_list_vertices(vertices *, int);/*verificación de que todo ok*/
triangulos *crear_lista_de_triangulos(char[], int *, int *); /*LISTA DE TRIANGULOS CON SUS TRES VERTICES*/
void verificar_list_triangulos(triangulos *, int, int); /*verificar que todo ok*/
void imprimir_list_triangulos(triangulos *); /*IMPRIME LISTA DE TRIANGULOS CON SUS VERTICES*/
/*ESTA FUNCIÓN BUSCA LOS TRIANGULOS QUE POSEAN DETERMINADO VERTICE Y LOS COLOCA EN UNA LISTA*/
triang_asociado_a_vert *buscar_list_triang(triangulos *, int, int*);
/*PARA CADA VERTICE SE CREARÁ UNA LISTA MEDIANTE LA FUNCIÓN ANTERIOR, LA FUNCIÓN
MATRIZ_DE_LISTAS TOMA ESAS LISTA Y LAS ALMACENA COMO SI FUERA UNA MATRIZ DONDE
CADA FILA ES UNA LISTA*/
matriz *matriz_de_listas(vertices *, triangulos *, int*);
void imprimir_listas_trian_asoc_vert(matriz *); /*IMPRIME TODAS LAS LISTAS DE LA MATRIZ*/
/*SE CREA UNA LISTA PARA CADA TRIANGULO DE SUS VECINOS*/
void lista_triang_vecino(int, int, triangulos *, triangulos *,vecinos **, int *);
/*LAS LISTAS GENERADAS CON LA FUNCIÓN ANTERIOR SE ALMACENAN EN UNA ESTRUTURA QUE ASEMEJA A UNA MATRIZ*/
mat_vec *matriz_triangulos_vecinos(triangulos *, int *);
/*IMPRIMIR LAS LISTAS DE TRIANGULOS CON SUS VECINOS*/
void imprimir_lista_triang_vecinos(mat_vec *);
/*LISTA DE LOS VALORES DE INDICES DE CALIDAD PARA CADA TRIANGULO*/
list_i_c *indices_calidad(vertices *, triangulos *, int *);
/*CALCULA MEDIANTE LOS VATICES Y SUS COORDENADAS X Y Y EL LADO DE UN TRIANGULO
(DISTANCIA ENTRE UN VERTICE Y OTRO)*/
double calcular_lado(vertices *, int, int);
/*IMPRIME LA LISTA DE VALORES DE INDICES DE CALIDAD DE CADA TRIANGULO*/
void imprimir_list_indice_calidad(list_i_c *);
/*VERIFICA SI LA TRIANGULACIÓN ES DE DELAURIE O NO*/
int delaunay(triangulos *, vertices *);
/*DADO UN TRIANGULO DEVUELVE SUS VERTICES */
void devolver_vertices(triangulos *, int, int *, int *, int *);
/*CALCULA LAS COORDENADAS DEL CENTRO DEL CIRCULO CIRCUNSCRITO AL TRIANGULO*/
void centro_circulo_circunscrito(double, double,double,double,double, double, double *, double *, int *);
/*VERIFICA SI LA TRIANGULACIÓN OBTENIDA TIENE UNA ARISTA ILEGAL, SE USA EN LA FUNCIÓN DELAUNAY */
int arista_ilegal (double,  double, double, double, double, double, int, int, int,int);
/*errorres que se pueden producir durante la ejecución*/
void error(int);
/*fin prototipos*/

main(void)
{
 vertices *list_vert; /*PUNTERO A PRINCIPIO DE LISTA DE VERTICES*/
 triangulos *list_triang; /*PUNTERO A PRINCIPIO DE LISTA DE TRIANGULOS*/
 matriz *matriz_triang_asoc_vert; /*PUNTERO A PRINCIPIO DE MATRIZ DE LISTAS DE
 TRIANGULOS ASOCIADOS A VERTICES*/
 mat_vec *list_tring_vecinos;  /*PUNTERO A PRINCIPIO MATRIZ DE LISTAS DE TRIANGULOS VECINOS*/
 list_i_c *lista_indices_calidad; /*PUNTERO A PRINCIPIO DE LISTA DE INDICES DE CALIDAD*/
 char nombre[25];/*NOMBRE DE ARCHIVOS DE DATOS*/
 char nombre2[25];
 int e, num_vert, num_triang;/*E=ERROR, NUM=NUM VERTICES O NUM DE TRIANGULOS*/

 clrscr(); /*limpia pantalla*/
 printf("\n");  /*MENU*/
 printf("\n\t\t**********************************************");
 printf("\n\t\t*                                            *");
 printf("\n\t\t*         ---  TRIANGULACION  ---            *");
 printf("\n\t\t* OPERACIONES:                               *");
 printf("\n\t\t*       1.- IMPRESION DE DATOS DE ENTRADA    *");
 printf("\n\t\t*                                            *");
 printf("\n\t\t*       2.- PARA CADA VERTICE SE IMPRIME LOS *");
 printf("\n\t\t*      TRIANGULOS QUE POSEEN DICHO VERTICE   *");
 printf("\n\t\t*                                            *");
 printf("\n\t\t*       3.- PARA CADA TRIANGULO SE IMPRIMEN  *");
 printf("\n\t\t*              LOS TRIANGULOS VECINOS        *");
 printf("\n\t\t*                                            *");
 printf("\n\t\t*       4.- PARA CADA TRIANGULO SE IMPRIMEN  *");
 printf("\n\t\t*              LOS INDICES DE CALIDAD        *");
 printf("\n\t\t*                                            *");
 printf("\n\t\t*       5.- VERIFICAR SI LA TRIANGULACION    *");
 printf("\n\t\t*               ES DE TIPO DELAUNAY          *");
 printf("\n\t\t*                                            *");
 printf("\n\t\t**********************************************");
 printf("\n\n"); /* ESPACIO;*/
 /*PIDO DATOS POR TECLADO*/
 printf("\t\tNOMBRE DE ARCHIVOS DE DATOS: \n");
 printf("\t\tARCHIVO DE VERTICES: ");
 scanf("%s",nombre);
 printf("\n\t\tARCHIVO DE TRIANGULOS: ");
 scanf("%s",nombre2);
 clrscr(); /*limpia pantalla*/
 /*CREO E IMPRIMO LISTA DE VERTICES*/
 list_vert=crear_lista_de_vertices(nombre, &e, &num_vert);
 error(e);
 verificar_list_vertices(list_vert,num_vert);
 imprimir_list_vertices(list_vert);
 /*CREO E IMPRIMO LISTA DE TRIANGULOS*/
 list_triang=crear_lista_de_triangulos(nombre2, &e, &num_triang);
 error(e);
 verificar_list_triangulos(list_triang,num_triang,num_vert);
 imprimir_list_triangulos(list_triang);
  /*LISTAS DE TRIANGULOS QUE TIENEN EN COMUN UN DETERMINADO VERTICE */
 matriz_triang_asoc_vert=matriz_de_listas(list_vert,list_triang, &e);
 error(e);
 imprimir_listas_trian_asoc_vert(matriz_triang_asoc_vert);
 /*LISTAS DE TRIANGULOS VECINOS*/
 list_tring_vecinos=matriz_triangulos_vecinos(list_triang,&e);
 error(e);
 imprimir_lista_triang_vecinos(list_tring_vecinos);

 lista_indices_calidad=indices_calidad(list_vert,list_triang, &e);
 error(e);
 imprimir_list_indice_calidad(lista_indices_calidad);

 e=delaunay(list_triang,list_vert);
 error(e);
 if (e==0)
 {
  printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
  printf("\n\n\t\t\t\t** FIN DE PROGRAMA ** ");
  printf("\n\t\t\t       <------------> ");
  printf("\n\t\t   LA TRIANGULACIÓN ES DE TIPO DELAUNARY!!!\n\n");
  printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
  printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
 } 
 getch();

}
/*_______________LLENAR LA LISTA DE VERTICES CON LOS VALORES____________________*/
/*ENTRADA: nombre del archivo de datos
  SALIDA: puntero a cabeza de la lista, error (diferente de cero en caso de error)
  num: número de vertices leídos*/
vertices *crear_lista_de_vertices(char nombre[], int *error, int *num)
{
 FILE *fp;  /*apuntador a archivo de datos*/
 vertices *principio=NULL, *elemento;
 int tamano=sizeof(vertices), i;   /*define para tamano*/
 *error=0; /*inicialización*/
 *num=1;
 /*________________________________________________________________________*/
 /* lectura del nombre del archivo */
 if ((fp = fopen(nombre,"r"))==NULL)
 {
  *error=3;  /*NO SE ENCUENTRA ARCHIVO ESPECIFICADO*/
  return(principio);
 }
 /*_______________________________________________________________________*/
 /* crear la primera estructura */
 if((principio=elemento=(vertices*)malloc(tamano))==NULL)
 {
  *error=4;   /*MEMORIA NO ASIGNADA*/
  return(principio);
 }
 if((fscanf(fp,"%d",&elemento->vertice))==EOF)
 {
  *error=5;   /*archivo vacio*/
  return(principio);
 }

 if((elemento->coordenadas=(double*)malloc(2*sizeof(double)))==NULL)
 {
  *error=4;   /*MEMORIA NO ASIGNADA*/
  return(principio);
 }
 if((fscanf(fp,"%lf",&elemento->coordenadas[0]))==EOF)
 {
  *error=6;   /*archivo incompleto*/
  return(principio);
 }
 if((fscanf(fp,"%lf",&elemento->coordenadas[1]))==EOF)
 {
  *error=6;   /*archivo incompleto*/
  return(principio);
 }
 elemento->siguiente = NULL;

 while(fscanf(fp,"%d",&i)!=EOF)   /*mientras no se llegue el final del archivo se leen los datos*/
 {
  if((elemento=elemento->siguiente=(vertices*)malloc(tamano))==NULL)
  {
   *error=4;   /*MEMORIA NO ASIGNADA*/
  return(principio);
  }
  elemento->vertice=i;
  if((elemento->coordenadas=(double*)malloc(2*sizeof(double)))==NULL)
  {
  *error=4;   /*MEMORIA NO ASIGNADA*/
  return(principio);
  }
  if((fscanf(fp,"%lf",&elemento->coordenadas[0]))==EOF)
  {
  *error=6;   /*archivo incompleto*/
  return(principio);
  }
  if((fscanf(fp,"%lf",&elemento->coordenadas[1]))==EOF)
  {
  *error=6;   /*archivo incompleto*/
  return(principio);
  }
  elemento->siguiente = NULL;
  (*num)++;  /*contador numero de vertices*/
 }
    fclose(fp);/*cierro archivo*/
    return(principio);  /*me devuelvo al programa principal*/
}
/*_____________FIN FUNCIÓN CREAR LISTA DE VERTICES___________________________________*/

/*_______________LLENAR LA LISTA DE TRIANGULOS CON LOS VALORES____________________*/
 triangulos *crear_lista_de_triangulos(char nombre[], int *e, int *num)
{
 FILE *fp; /*puntero a file*/
 triangulos *principio=NULL, *elemento;

 int tamano=sizeof(triangulos), i;   /*define para tamano*/
 /*INICIALIZACIÓN*/
 *e=0; *num=1;
 /*________________________________________________________________________*/
 /* lectura del nombre del archivo */
 if ((fp = fopen(nombre,"r"))==NULL)
 {
  *e=8;   /*no se puede abrir archivo*/
  return(principio);
 }
 /*_______________________________________________________________________*/
 /* crear la primera estructura */
 if((principio=elemento=(triangulos*)malloc(tamano))==NULL)
 {
  *e=4; /*memoria no asignada*/
  return(principio);
 }
 if((fscanf(fp,"%d",&elemento->triangulo))==EOF)
 {
  *e=5;   /*archivo vacio*/
  return(principio);
 }
 if((elemento->num_vertice=(int*)malloc(3*sizeof(int)))==NULL)
 {
  *e=4;  /*memoria no asignada*/
  return(principio);
 }
 if((fscanf(fp,"%d",&elemento->num_vertice[0]))==EOF)
 {
  *e=9; /*archivo incompleto*/
  return(principio);
 }
 if((fscanf(fp,"%d",&elemento->num_vertice[1]))==EOF)
 {
  *e=9;     /*archivo incompleto*/
  return(principio);
 }
 if((fscanf(fp,"%d",&elemento->num_vertice[2]))==EOF)
 {
  *e=9;  /*archivo incompleto*/
  return(principio);
 }

 elemento->siguiente = NULL;

 while(fscanf(fp,"%d",&i)!=EOF)
 {
  elemento=elemento->siguiente=(triangulos*)malloc(tamano);
  elemento->triangulo=i;
  if((elemento->num_vertice=(int*)malloc(3*sizeof(int)))==NULL)
  {
  *e=4;  /*falla en asinación de memoria*/
  return(principio);
  }
  if((fscanf(fp,"%d",&elemento->num_vertice[0]))==EOF)
  {
  *e=9;  /*archivo incompleto*/
  return(principio);
  }
  if((fscanf(fp,"%d",&elemento->num_vertice[1]))==EOF)
  {
  *e=9;  /*archivo incompleto*/
  return(principio);
  }
  if((fscanf(fp,"%d",&elemento->num_vertice[2]))==EOF)
  {
   *e=9;  /*archivo incompleto*/
   return(principio);
  }
  elemento->siguiente = NULL;
  (*num)++;
 }

    fclose(fp);  /*se cierra archivo*/
    return(principio); /*se retorna a programa principal*/
}
/*_____________FIN FUNCIÓN CREAR LISTA DE TRIANGULOS___________________________________*/
/*________________________IMPRIMIR LISTA DE VERTICES_________________________*/
 void imprimir_list_vertices(vertices *principio)
 {
  vertices *elemento;
  printf("\n\t\tLISTA DE VERTICES:");
  printf("\n\n\t\tVERTICE \t\tCOORDENADAS\n");
  printf("\n\t\t\t\t\tX\tY\n\n");
  for (elemento=principio;elemento;elemento=elemento->siguiente)
  {
   printf("\t\t%d",elemento->vertice);
   printf("\t\t%lf\t%lf\n\n",elemento->coordenadas[0],elemento->coordenadas[1]);
  }
  printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
 }
 /*_____________________VERIFICAR_LIST-VERTICES_________________________________________*/
 void verificar_list_vertices(vertices *principio, int num)
 /*verifica que los datos leídos por archivo de vértices cumplan con que los valores
 de los vértices estén dentro del rango de 0 y t-1, con
 t número total de vértices, y que no hallan vértices repetidos, en caso de error
 se termina la ejecución del programa.*/
 {
  vertices *elemento, *elem;
  
  for (elem=principio;elem;elem=elem->siguiente)
  {
    for (elemento=principio;elemento;elemento=elemento->siguiente)
    {
     if(elem==elemento)
      continue;
     if((elem->vertice)==(elemento->vertice)) /*repetidos*/
     {
      printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
      printf("\n\n\t\t\t\t** ERROR ** ");
      printf("\n\t\t\t       <------------> ");
      printf("\n\t\t         VERTICES REPETIDOS \n\n");
      printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
      printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
      getch();
      exit(1);
     }
    }
     if((elem->vertice<0) || (elem->vertice>num-1)) /*fuera del rango*/
     {
      printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
      printf("\n\n\t\t\t\t** ERROR ** ");
      printf("\n\t\t\t       <------------> ");
      printf("\n\t\t     RANGO DE VERTICES NO ESTA ENTRE 0 Y %d \n\n", num-1);
      printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
      printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
      getch();
      exit(1);
     }
  }
 }
 /*___________________________________________________________________________*/
  /*_____________________VERIFICAR_LIST-triangulos_________________________________________*/
 void verificar_list_triangulos(triangulos *principio, int num_triang, int num_vert)
 /*verifica que los datos leídos por archivo de triàngulos cumplan con que los valores
 de los triángulos estén dentro del rango de 0 y t-1, con
 t número total de triángulos,que no hallan triángulos repetidos, que los
 vertices de los triangulos esrten dentro del rango y que los vertices de determinado triangulos
 no sean iguales, en caso de error se termina la ejecución del programa.*/
 {
  triangulos *elemento, *elem;
  int i,j;
  for (elem=principio;elem;elem=elem->siguiente)
  {
    for (elemento=principio;elemento;elemento=elemento->siguiente)
    {
     if(elem==elemento)
      continue;
     if((elem->triangulo)==(elemento->triangulo)) /*repetidos*/
     {
      printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
      printf("\n\n\t\t\t\t** ERROR ** ");
      printf("\n\t\t\t       <------------> ");
      printf("\n\t\t        TRIANGULOS REPETIDOS \n\n");
      printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
      printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
      getch();
      exit(1);
     }
    }
     if((elem->triangulo<0) || (elem->triangulo>num_triang-1)) /*fuera del rango*/
     {
      printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
      printf("\n\n\t\t\t\t** ERROR ** ");
      printf("\n\t\t\t       <------------> ");
      printf("\n\t\t     RANGO DE TRIANGULOS NO ESTA ENTRE 0 Y %d \n\n", num_triang-1);
      printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
      printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
      getch();
      exit(1);
     }
     for (i=0; i<3;i++)/*vertices de los triangulos fuera de rango*/
     {
      if((elem->num_vertice[i]<0) || (elem->num_vertice[i]>=num_vert)) /*fuera del rango*/
      {
       printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
       printf("\n\n\t\t\t\t** ERROR ** ");
       printf("\n\t\t\t       <------------> ");
       printf("\n\t\tRANGO VERTICES DE LOS TRIANGULOS NO ESTA ENTRE 0 Y %d \n\n", num_vert-1);
       printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
       printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
       getch();
       exit(1);
      }
     }
     for (i=0; i<2;i++)/*vertices del triangulo iguales*/
      for (j=i+1;j<3;j++)
      {
       if(elem->num_vertice[i]==elem->num_vertice[j])
       {
       printf("\ni=%d, j=%d",i,j);
       printf("elem->num_vertice[%d]=%d==elem->num_vertice[%d]=%d",i,elem->num_vertice[i],j,elem->num_vertice[j]);
       printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
       printf("\n\n\t\t\t\t** ERROR ** ");
       printf("\n\t\t\t       <------------> ");
       printf("\n\t\tVERTICES DEL TRIANGULO IGUALES \n\n");
       printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
       printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
       getch();
       exit(1);
      }
     }
  }
 }
 /*_________________IMPRIMIR LISTA DE TRIANGULOS_____________________________*/

  void imprimir_list_triangulos(triangulos *principio)
 {
  triangulos *elemento;
  printf("\n\t\tLISTA DE TRIANGULOS:");
  printf("\n\n\t\tTRIANGULO \t\tVERTICES:\n");
  for (elemento=principio;elemento;elemento=elemento->siguiente)
  {
   printf("\n\t\t%d",elemento->triangulo);
   printf("\t\t\t\t%d, %d, %d\n",elemento->num_vertice[0],elemento->num_vertice[1],elemento->num_vertice[2]);
  }
  printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
 }
 /*_______________________BUSCAR_LIST_TRIANG___________________________________*/
 /*__CREA las listas de triangulos que contienen determinado vertice______*/

  triang_asociado_a_vert *buscar_list_triang(triangulos *principio, int item, int *error)
{         /*SALIDA: UN PUNTERO A CABEZA DE LA LISTA Y UN VALOR ENTERO PARA EL ERRRO*/
 triangulos *elemento;
 triang_asociado_a_vert *princ=NULL,*elem;
 int h=0,i;
 *error=0;
 for(elemento=principio; elemento;elemento=elemento->siguiente)  /*RECORRO LA LISTA CON LOS DATOS DE TRIANGULOS*/
 {
  for(i=0;i<3;i++)
   if(item==elemento->num_vertice[i]) /*VEO SI LOS VERTICES COINCIDEN*/
   {
    h++;
    if (h==1)  /*... Y ENTONCES CREO LA LISTA*/
    {
     if((princ=elem=(triang_asociado_a_vert*)malloc(sizeof(triang_asociado_a_vert)))==NULL)
     {
      *error=4;  /*ERROR DE ASIGNACIÓN DE MEMORIA*/
      return(princ);
     }
     elem->vert=item;   /*ASIGNO LOS VALORES DE LA LISTA*/
     elem->triang=elemento->triangulo;
     elem->siguiente=NULL;
    }
    else
    {
     if((elem=elem->siguiente=(triang_asociado_a_vert*)malloc(sizeof(triang_asociado_a_vert)))==NULL)
     {
      *error=4; /*ERROR DE ASIGNACIÓN DE MEMORIA*/
      return(princ);
     }
     elem->vert=item; /*ASIGNO LOS VALORES DE LA LISTA*/
     elem->triang=elemento->triangulo;
     elem->siguiente=NULL;
    }
   }
 }
 return(princ); /*RETORNO A PP*/
}

/*_________________________MATRIZ DE LISTAS_______________________________________*/
/*PARA CADA VERTICE HAY UN LISTA DE TRINAGULOS QUE LO CONTIENEN*/
 matriz *matriz_de_listas(vertices *list_vert, triangulos *list_triang, int *error)
 {  /*ACOMODA LAS LISTA ANTERIORES DENTRO DE UNA ESPECIE DE MATRIZ, DONDE LAS FILAS
 SON LAS MISMA LISTAS*/
 /*SALIDA= APUNTADOR A PRICIPIO DE MATRIZ Y UN ENTERO CON UN VALOR PARA  LA FUNCIÓN ERROR*/
  int h=0,item, e=0;
  triang_asociado_a_vert *list_triang_asoc_vert;  /*apuntador a lista
                                             de trinagulos que contienen a vertice*/
  vertices *elem_vert; /*apuntador a lista de vertices*/
  matriz *m=NULL,*ma;  /*apuntadores para crear la estructura parecida a matriz*/
  error=0;
  for (elem_vert=list_vert;elem_vert;elem_vert=elem_vert->siguiente)
  {         /*recorro la lista de vertices*/
   item=elem_vert->vertice; /*vertice de turno*/
   h++;
   if (h==1)
   { /*llamo a la función que me creara la lista de tringulos que continen a este vertice*/
    list_triang_asoc_vert=buscar_list_triang(list_triang, item, &e);
    if(e)
    {
     *error=e;        /*error grave hay que salir, no se asigno memoria*/
     return(m);
    }
    if(!list_triang_asoc_vert)  /*si la lista resulto nula, nos asguramos de hacer h=0
                                 para que la siguiente lista sea la primera*/
    {
     h=0;
     continue;
    }
    if((m=ma=(matriz*)malloc(sizeof(matriz)))==NULL)/*asigno memoria*/
    {
     *error=4; /*error de asignación de memoria*/
     return(m);
    }
    ma->comienzo=list_triang_asoc_vert;  /*asigno valores*/
    ma->siguiente=NULL;
   }
   else   /*si este no es el primer elemento de la matriz*/
   {       /*creo la lista de triangulos con el mismo vertice*/
    list_triang_asoc_vert=buscar_list_triang(list_triang, item, &e);
    if(e)
    {
     *error=e;/*error de asignación de memoria*/
     return(m);
    }
    if(!list_triang_asoc_vert) /*en caso de que no se encuentren triangulos
                                para este vertice no se crea la lista*/
     continue;
    if((ma=ma->siguiente=(matriz*)malloc(sizeof(matriz)))==NULL)
    {
     *error=4; /*error de asignación memoria*/
     return(m);
    }
    ma->comienzo=list_triang_asoc_vert; /*asignación de valores*/
    ma->siguiente=NULL;
   }
  }
  return(m);
 }
 /*__________________________________________________________________________*/
  /*IMPRIMIR LAS LISTA DE TRINGULOS ASOCIADOS A DETERMINADO VERTICE*/
void imprimir_listas_trian_asoc_vert(matriz *m)
 {
  matriz* ma; /*APUNTADOR A LA MATRIZ*/
  triang_asociado_a_vert *elem;  /*APUNTADOR A LA LISTA*/
  printf("\n\t\tLISTA DE VERTICES Y TRIANGULOS QUE LOS CONTIENEN:\n ");
  printf("\n\t\tVERTICE\t\tTRIANGULOS\n\n");
  for (ma=m;ma;ma=ma->siguiente) /*RECORRO LA ESTRUCTURA*/
  {
    printf("\t\t %d",(ma->comienzo)->vert);
    printf("\t\t");
    for(elem=ma->comienzo;elem;elem=elem->siguiente)
    {
     printf("%d ",elem->triang);
    }
    printf("\n");
  }
  printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
 }
  /*________________MATRIZ TRIANGULOS_VECINOS__________________________________*/
 /*ESPECIE DE MATRIZ QUE TIENE POR FILAS LISTAS QUE CONTIENEN LOS TRIANGULOS VECINOS DE
 UN DETERMINADO TRIANGULO EN LA TRIANGULACIÓN*/
 mat_vec *matriz_triangulos_vecinos(triangulos *list_triang, int*error)
 {
  int h=0, punto1,punto2, i, j, e=0;
  triangulos *elemento; /*apuntadores*/
  vecinos *lista_vecinos;
  mat_vec *m=NULL, *ma;
  error=0;
  for(elemento=list_triang;elemento;elemento=elemento->siguiente)
  {                            /*recorro lista de triangulos para hallar
                               los triangulos que comparte el mismo lado*/
   lista_vecinos=NULL; /*apuntador a NULL*/
   for (i=0;i<2;i++)
    for (j=i+1;j<3;j++)
    { /*veo cuales son los vertices del triangulo al cual quiero hallarles sus vecinos*/
     punto1=elemento->num_vertice[i];/*Por claridad hago una asignación*/
     punto2=elemento->num_vertice[j];
     /*esta función halla los triangulos que son vecinos a este triangulo y los coloca
     en una lista*/
     lista_triang_vecino(punto1,punto2,elemento,list_triang, &lista_vecinos, &e);
    }
    if(e)
    {
     *error=e; /*error dentro de la función lista_triang_vecino*/
     return(m);
    }
   if(h==0 && lista_vecinos!=NULL)
   {    /*si esta es la primera lista y esta lista NO ES NULA entonces
   serà la primera fila de la matriz, es decir el principio de la matriz*/
    h++; /*cuando h ya no sea cero indica que la cabecera de la matriz ya apunta
    a una primera lista*/
    if((m=ma=(mat_vec*)malloc(sizeof(mat_vec)))==NULL)
    {
     *error=4; /*falló asignación de memoria*/
     return(m);
    }
    ma->comienzo=lista_vecinos;  /*asignacion de valores*/
    ma->siguiente=NULL;
   }
   else if(h!=0 && lista_vecinos!=NULL)  /*si no es la primera lista, pero no es nula*/
   {
    if((ma=ma->siguiente=(mat_vec*)malloc(sizeof(mat_vec)))==NULL)
     {
     *error=4; /*falló asignación de memoria*/
     return(m);
    }
    ma->comienzo=lista_vecinos; /*asignación*/
    ma->siguiente=NULL;
   }
  }
  return(m); /*retorno al programa principal*/
 }

/*_______________________________lista_triang_vecino__________________________*/
/*halla cuales triangulos tienen dos vertices comúnes (un lado en común)y luego crea una
lista con ellos*/
void lista_triang_vecino(int punto1, int punto2, triangulos *puntero, triangulos *list_triang,vecinos **cabeza, int *error)
{
 vecinos *nodo_vecino;
 triangulos *elem;
 int punto3,punto4, i,j,k;
 *error=0;
 /*quien comparte el lado abs(punto1-punto2), sólo puede ser un triangulo*/
 for(elem=list_triang;elem;elem=elem->siguiente)
 {
  if (elem==puntero)  /*si llos punteros son iguales nos referemos al triangulo al cual
                     le queremos hallar los vecinos, así que lo ignoramos*/
    continue;
  for (i=0;i<2;i++)
  {
    for (j=i+1;j<3;j++)
    {
     k=0;            /*comparamos los vertices y cuando encontremos los que sean iguales salimos
                       y creamos la lista nodo por nodo, introduciendo desde principio, (note que
                       es diferente a como se había acostubrado hacer)*/
     punto3=elem->num_vertice[i];/*Por claridad hago una asignación*/
     punto4=elem->num_vertice[j];
     if((punto3==punto1 && punto4==punto2) ||(punto4==punto1 && punto3==punto2))
     {
      k=1;
      break;/*si encuentro el lado común salgo del ciclo*/
     }
    }
   if(k==1)
    break;
  }
  if(k==1) /*este k indica que se encontro un triangulo que compate el mismo lado
            así que si la lista ya esta creada se introduce el nuevo nodo desde el principio
            sino se crea*/
  {
   if((nodo_vecino=(vecinos*)malloc(sizeof(vecinos)))==NULL)
    *error=4; /*error asignado memoria*/
   else /*asigno e introduzco el nodo en la lista*/
   {
    nodo_vecino->triang=puntero->triangulo;
    nodo_vecino->triang_vecinos=elem->triangulo;
    nodo_vecino->siguiente=*cabeza;
    *cabeza=nodo_vecino;  /*coloco los nuevos nodos en la cabeza de la lista, esto
    lo hice considerando que no importa el orden*/
    break;
   }
  }
 }
}
/*_______________________________________________________________________________*/
/*IMPRIMIR LISTA DE TRIANGULOS VECINOS*/
void imprimir_lista_triang_vecinos(mat_vec *m)
 {
  mat_vec* ma;
  vecinos *elem;
  printf("\n\t\tTRIANGULOS VECINOS:\n\n ");
  printf("\t\tTRIANGULOS\t\tTRIANGULOS VECINOS\n\n");
  for (ma=m;ma;ma=ma->siguiente)
  {
    printf("\t\t%d",(ma->comienzo)->triang);
    printf("\t\t\t");
    for(elem=ma->comienzo;elem;elem=elem->siguiente)
    {
     printf("%d ",elem->triang_vecinos);
    }
    printf("\n");   
  }
  printf("\n\t\t*------ ENTER PARA CONTINUAR *-------\n\n\t\t");
   getch();
 }
  /*____________________________________________________________________________*/
 /*lista en la cual para cada triangulo esta el indice de calidad*/
 list_i_c *indices_calidad(vertices *list_vert, triangulos *list_triang, int *error)
 {
  triangulos *elemento;
  list_i_c *inicio_list_ic=NULL, *aux;
  int i, j, k,vert1, vert2, h=0;
  double *vector_lados, lado_mayor, p, r;
  *error=0;
  /*asignación dinámica de memoria para un vector de dimensión 3*/
  if((vector_lados=(double*)malloc(3*sizeof(double)))==NULL)
  {
   *error=4; /*error de asignacion de memoria*/
    return(inicio_list_ic);
  }
  /*se recorre la lista de triangulos para ver los vertices*/
  for (elemento=list_triang; elemento; elemento=elemento->siguiente)
  {
   /*inicilización*/
   for (k=0;k<3;k++)vector_lados[k]=0;
   k=0; p=0; r=0;
   for (i=0;i<2;i++)
    for (j=i+1;j<3;j++)   /*calculando el valor de cada uno de los lados del */
    {                     /*triangulo de turno*/
     vert1=elemento->num_vertice[i];
     vert2=elemento->num_vertice[j];
     vector_lados[k]=calcular_lado(list_vert,vert1,vert2);  /*FUNCIÓN QUE MEDIANTE DOS VERTICES
                                                           DEVUELVE EL VALOR DEL LADO*/
     /*semiperimetro del triángulo p=(a+b+c)/2 (suma de los elementos del vector),lados
    del triangullo, màs abajo p/2*/
     p=p+vector_lados[k];
     k++;
    }
    p=p/2; /*valor del semiperimetro*/
    /*ver cual lado es el lado mayor */
    lado_mayor=vector_lados[0];
    for (k=0;k<2;k++)
    {
     if (lado_mayor<vector_lados[k+1])
        lado_mayor=vector_lados[k+1];
    }
     if(!p || !lado_mayor)
     {
      *error=10;  /*evitar división por cero*/
      return(inicio_list_ic);
     }

    /*FÓRMULA DE HERÓN: para hallar el radio de un círculo inscrito en un triangulo*/
    r=((sqrt(p*(p-vector_lados[0])*(p-vector_lados[1])*(p-vector_lados[2])))/p);
    /*p diferente de cero?*/
   /*acomodo los valores en una lista*/
   if(h==0)
   {
    h++;
    if((inicio_list_ic=aux=(list_i_c*)malloc(sizeof(list_i_c)))==NULL)
    {
     *error=4; /*error de asignación de memoria*/
      return(inicio_list_ic);
    }
    aux->triangulo=elemento->triangulo;/*asigno los valores*/
    aux->indice_calidad=r/lado_mayor; /*diferente de cero?*/
    aux->siguiente=NULL;
   }
   else
   {    /*asigno valores al sigiente elemnto de la lista*/
    aux=aux->siguiente=(list_i_c*)malloc(sizeof(list_i_c));
    aux->triangulo=elemento->triangulo;
    aux->indice_calidad=r/lado_mayor; /*diferente de cero?*/
    aux->siguiente=NULL;
   }
  }
  return(inicio_list_ic); /*retorna a pp*/
 }
/************** ____________CALCULAR LADO______________________ ********/
/*CALCULO POR MeDIO DE LAS COORDENADAS X Y Y DE LOS VERTICES DE LOS LADOS DE
LA LONGITUD DE LOS LADOS*/
 double calcular_lado(vertices *list_vert, int vert1, int vert2)
 {
  vertices *elemento;
  double x1, x2,y1,y2;
            /*HALLO LAS COORDENADAS DE LOS VERTICES*/
  for (elemento=list_vert; elemento; elemento=elemento->siguiente)
  {
   if((elemento->vertice)==vert1)
   {
    x1=elemento->coordenadas[0];  /*NECESITO DOS VERTICES PARA EL CALCULO*/
    y1=elemento->coordenadas[1];
   }
   if((elemento->vertice)==vert2)
   {
    x2=elemento->coordenadas[0];
    y2=elemento->coordenadas[1];
   }
  }
   return(sqrt(pow((x1-x2),2)+pow((y1-y2),2))); /*RETORNA EL LADO A LA FUNCIÓN*/
 }
 /*_________________IMPRIME INDICE DE CALIDAD CON SU RESPECTIVO TRIANGULO_________*/
  void imprimir_list_indice_calidad(list_i_c *principio)
 {
  list_i_c *elemento;
  printf("\n\n\t\tVALORES INDICES DE CALIDAD:\n\n");
  printf("\n\t\tTRIANGULO\t\tINDICE DE CALIDAD\n");
  for (elemento=principio;elemento;elemento=elemento->siguiente)
  {
   printf("\t\t %d ",elemento->triangulo);
   printf("\t\t\t %lf\n\n",elemento->indice_calidad);
  }
 }
 /*______________________FUNCIÓN X_Y_______________________________________*/
 /*para un vertice dado se hallan sus coordenadas en x y y*/
 void x_y(vertices *list_vert, int vert, double *x, double *y)
 {
  vertices *elemento;

  for (elemento=list_vert; elemento; elemento=elemento->siguiente)
  {
    if((elemento->vertice)==vert)
    {
     *x=elemento->coordenadas[0];
     *y=elemento->coordenadas[1];
    }
  }
 }
 /*__________________FUNCIÓN DE DELAUNARY_______________________________________*/
 /*si esta función devuelve un valor entero igual a 2, entoces la triangulación
 no es de delaunary*/
 int delaunay(triangulos *list_triang, vertices *list_vert)
 {

  triangulos *elem, *elemento;
  int t, v1,v2,v3,vv1,vv2,vv3, ver1, ver2, ver3, e=0;
  double x1,x2,x3,y1,y2,y3,xp1,yp1,xp2,yp2,xp3,yp3, xc, yc;

  for (elemento=list_triang;elemento;elemento=elemento->siguiente) /*recorro la lista
  de triangulos */
  {
   e=0;
   t=elemento->triangulo;
   devolver_vertices(list_triang,t,&v1,&v2,&v3); /*función que dado un triangulo devuelve los vertices del mismo*/
   x_y(list_vert,v1,&x1,&y1); /*dado un vertice devuelve las coordenadas de x y */
   x_y(list_vert,v2,&x2,&y2);
   x_y(list_vert,v3,&x3,&y3);
   /*hallo las coordenadas del centro del triangulo cicunscrito*/
   centro_circulo_circunscrito(x1,x2,x3,y1,y2,y3,&xc,&yc,&e); /*parametros de salida
   xc y yc (coordenadas del centro del círculo circunscrito) y e(error)*/
   if(e==1)
    return(e); /*error:puntos coincidentes*/
  /*EN PRINCIPIO PARA LA VERIFICACION DE LA CONDICIÓN SOLO HABIA TOMADO LOS TRIÁNGULOS
  VECINOS, PUES PARA QUE EL CÍCULO CIRCUSCRITO TOME VERTICES MÀS ALEJADOS DEBE TOMAR
  PRIMERO LOS VERTICES VECINOS Y ESO SERÍA SUFICIENTE PARA QUE LA TRIANGULACIÓN
  YA NO FUERA DE DELAUNARY, PERO SI PENSAMOS EN VERTICES QUE CORTEN UN LADO DEL
  TRIANGULO AL CUAL LE VERIFICAMOS LA CONDICIÓN, EL VERTICE QUE CORTE ESE LADO
  NO ESTARÀ ENTRE LOS VECINOS, ASÌ QUE AL COLOCAR SOLO LOS VECINOS EN CASO DE OCURRIR
  ESTO NOS ESTARIAMOS EQUIVOCANDO AL AFIRMAR QUE LA TRIANGULACIÓN ES DELAUNARY,
  ASÍ QUE TOCA USAR TODOS LOS VERTICES DE TODOS LOS TRIANGULOS, MÀS TRABAJO PARA LA MÀQUINA
  PERO ASÍ VERIFICAMOS DOS CONDICIONES(las últimas dos del punto 2 (ver informe al inicio)) */
   for(elem=list_triang;elem;elem=elem->siguiente)
   {
    if(elem==elemento) /*si es el apuntador del triangulo del circulo circunscrito
                       se obvia */
     continue;
    t=xp1=xp2=xp3=yp1=yp2=yp3=0;
    t=elem->triangulo;                          /*para un triangulo dado se devuelven los vertices*/
    devolver_vertices(list_triang,t,&vv1,&vv2,&vv3);
    x_y(list_vert,vv1,&xp1,&yp1);         /*coordenadas xy*/
    x_y(list_vert,vv2,&xp2,&yp2);                     /**/
    x_y(list_vert,vv3,&xp3,&yp3);
    ver1=arista_ilegal(xc,yc,x2,y2,xp1,yp1,vv1,v1,v2,v3);/*con esto vemos si las coordenasdas
    del vertice del triangulo de turno esta dentro del círculo circuscrito del triangulo
    al cual se le verifica la condición de DELAUNARY*/
    ver2=arista_ilegal(xc,yc,x2,y2,xp2,yp2,vv2,v1,v2,v3);
    ver3=arista_ilegal(xc,yc,x2,y2,xp3,yp3,vv3,v1,v2,v3);
    if (ver1==TRUE || ver2==TRUE || ver3==TRUE)
     return(2); /*NO ES UNA TRIANGULACIÓN DE DELAUNAY*/
   }
  }
  return(e);
 }
/*_____________________DEVOLVER_VERTICES__________________________________________*/
 void devolver_vertices(triangulos *list_triang, int t, int *v1, int *v2, int *v3)
 {    /*dado un triangulo buscar sus vertices en la lista de triangulos*/
  triangulos *elemento;
  for (elemento=list_triang;elemento;elemento=elemento->siguiente)
   {
    if(elemento->triangulo==t)
    {
      *v1=elemento->num_vertice[0];
      *v2=elemento->num_vertice[1];
      *v3=elemento->num_vertice[2];
    }
   }
 }
 /*__________________________________________________________________________________*/
 void centro_circulo_circunscrito(double x1, double x2,double x3,double y1,double y2, double y3, double *xc, double *yc, int *e)
 {
  double D;
  *e=0;
  *xc=0; *yc=0;
  /*chequeo que no hayan puntos que coincidan*/
  if((fabs(y1-y2)<EPSILON) && (fabs(y2-y3)<EPSILON))
    *e=1;  /*ERROR PUNTOS COINCIDENTES, ESTO NO PARECE SER UN TRIANGULO*/
  else
  {        /*COORDENADAS DEL CENTRO DEL CIRCULO CIRCUNSCRITO*/
  D=2*(y1*x3+y2*x1-y2*x3-y1*x2-y3*x1+y3*x2);
  *xc=(y2*x1*x1-y3*x1*x1-y2*y2*y1+y3*y3*y1+x2*x2*y3+y1*y1*y2+x3*x3*y1-y3*y3*y2-x3*x3*y2-x2*x2*y1+y2*y2*y3-y1*y1*y3)/D;
  *yc=(x1*x1*x3+y1*y1*x3+x2*x2*x1-x2*x2*x3+y2*y2*x1-y2*y2*x3-x1*x1*x2-y1*y1*x2-x3*x3*x1+x3*x3*x2-y3*y3*x1+y3*y3*x2)/D;
  }
 }
 /*______________________arista_ilegal_____________________________________________*/
 /*verificar si el punto xp yp es un arista ilegal o no (vertice ilegal: la circunfrenecia
 definida por los vértices de un triçengulo contiene otro punto de la triangulación sii
 el triangulo tiene una arista ilegal )*/
 int arista_ilegal (double xc,  double yc, double x2, double y2, double xp, double yp, int vv, int v1, int v2,int v3)
 {
  double x,y,z,k,rr1=0,rr2=0;

  if(vv==v1 || vv==v2 || vv==v3)/*VER QUE LOS VERTICES DEL TRIANGULO AL QUE SE LE VERIFICA
  LA CONDICIÓN NO SON LOS MISMOS DEL LOS OTROS TRIANGULOS */
    return(FALSE); /*SI SON LOS MISMO NO VERIFICAMOS LA CONDICIÓN PUES SON LOS MISMO VERTICES DEL CIRCULO
    ASÍ QUE DE UNA VEZ LE ASIGNAMOS FALSE*/

  x=x2-xc;
  y=y2-yc;       /*rr1 y rr2 radio al cuadrado*/
  rr1=x*x+y*y;
  z=xp-xc;
  k=yp-yc;         /*radio del punto del cual queremos saber si esta dentro o fuera del círculo*/
  rr2=k*k+z*z;      /*circunscrito*/
  return((rr2<rr1) ? TRUE : FALSE); /*si el radio al cuadrado de los puntos xp yp es menor
                                     que el radio al cuadrado de un punto sobre
                                      el cículo entoces tenemos un vètice dentro del
                                      circulo => tenemos una arita ilegal (TRUE)
                                      en caso contrario (FALSE)*/
 }

 /*_______________________ERROR________________________________________*/
 void error(int e)
 {
   if(e==1)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\tPUNTOS COINCIDENTES, ESTO NO PARECE SER UN TRIANGULO\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
  else if(e==2)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   LA TRIANGULACIÓN NO ES DE DALAUNAY\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
  else if (e==3)         /*errores que se pueden producir durante la ejecución */
  {                 /*del programa*/
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\tNO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO PARA VERTICES\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
  else if(e==4)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   MEMORIA NO ASIGNADA, FIN DE LA EJECUCION\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
   else if(e==5)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t              ARCHIVO VACIO!!!\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
   else if(e==6)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t     ARCHIVO DE VERTICES INCOMPLETO\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
   
   else if (e==8)         /*errores que se pueden producir durante la ejecución */
  {                 /*del programa*/
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\tNO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO PARA TRIANGULOS\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
  else if(e==9)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t     ARCHIVO DE TRIANGULOS INCOMPLETO\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
   else if(e==10)
  {
   printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
   printf("\n\n\t\t\t\t** ERROR ** ");
   printf("\n\t\t\t       <------------> ");
   printf("\n\t\t   DIVISION POR CERO, FIN DEL CALCULO!!!\n\n");
   printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
   printf("\n\t\t*------ ENTER PARA TERMINAR *-------\n\n\t\t");
   getch();
   exit(1);
  }
 }
