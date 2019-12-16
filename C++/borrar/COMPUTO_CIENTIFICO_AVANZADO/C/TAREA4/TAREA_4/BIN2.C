/* Erika Gomes
   0032832
   NOMBRE DEL PROGRAMA: BIN2.C
   DESCRIPCIÓN: Convierte un número decimal del tipo 0.xxxxx, de sólo 5 dígitos
   depués del punto decimal y lo transforma en un número binario. El programa
   sólo se detendrá al final de cada operación y le preguntará al usuario
   si desea realizar otra transformación o desea salir
   En caso de no ingresar un número decimal entre uno(1) y cero(0), el programa
   se detendrá advertirá el error y pedirá otro número, esto sucederá hasta que
   el usuario coloque un número correcto (tal como se le indica durante
   la ejecución).
*/
# include <stdio.h>  /* biblioteca estándar*/
# include <stdlib.h> /* para la conversión, función atof */
# include <conio.h>  /*indispensable para usar clrscr();*/
# include <string.h> /*para las funciones srtlen, strcat, getchar*/

 main ()
{
 double valor , valoraux, valor1;
 char leo[100],leo1[9]="22222222",cero='0',punto='.';
 char otra;
 int error, tam,condicion, opcion, i, j, entero[301][5], aux, sumo, k, bin[300];
 int salir, h, iguales,per;


 /*CICLO INFINITO*/
 condicion=1;
 while(condicion==1)
 {
   clrscr(); /*limpia pantalla*/
   printf("\n");  /*MENU*/
   printf("\n\t\t**********************************************");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                ---  MENU  ---              *");
   printf("\n\t\t* OPERACIONES:                               *");
   printf("\n\t\t*       0.- SALIR                            *");
   printf("\n\t\t*       1.- Transformacion a binario         *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t*                                            *");
   printf("\n\t\t**********************************************");
   printf("\n"); /* ESPACIO;*/

   printf("\n\t\tINTRODUZCA OPCION: ");
   scanf("%d",&opcion); /*LECTURA */


   if (opcion!=1)break;  /*opción diferente de 1 salir*/



  do    /*lee y verifica el dato introducido*/
  {
   error=0;  /*condición de entrada al ciclo, SI ERRROR= 1 entra al ciclo
              de lo contario no*/
   printf("\n\tINTRODUZCA NUMERO DECIMAL ENTRE 0 y 1 : ");
   scanf("%s",leo); /*leo el número introducido como una cadena de caracteres*/
   valor=atof(leo);  /*si no logra la conversión devuelve cero y se repite
                       el ciclo, volviendo a pedir otro valor*/
   valor1=valor;
   tam=strlen(leo); /*cantidad de carácteres en el array(cadena) de caracteres*/

   otra=leo[0];  /*primer caracter de la cadena de caracteres*/
   if (otra==cero) tam=tam-2;  /*si 0.xxxxx, al tamaño de la cadena se le resta
                                2 (0.), solo nos interesa el número de dígitos
                                después del punto*/
   else if (otra==punto) tam=tam-1;/*si .xxxxx al tam se le resta 1 (por el .)*/




   if (valor<=0.0 ||valor>=1.0)/*verifico que no sea ni menor igual a cero, ni
                               mayor o igual a 1*/
   {   /*MENSAJE DE ERROR*/
    printf("\n\n\t\t++++++++++++++++++++++++++++++++++++++++++++ ");
    printf("\n\t\t\t\t** ERROR ** ");
    printf("\n\t\t\t       <------------> ");
    printf("\n\t\t   RECUERDE!!! el numero esta entre 0 y 1\n");
    printf("\n\t\t++++++++++++++++++++++++++++++++++++++++++++ \n\n");
    error=1; /*SI SE PRODUCE UN ERROR SE REPITE EL CICLO*/
   }
  }while(error);

 /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
  if (tam>5) /*si el número de dígitos en la parte decimal es mayor que
              igual a seis, se advierte al usuario, qie se truncara la cifra*/
  {
   tam=5; /*se asigna el tamaño*/
   printf("\n\t\t ++++++++++++++++++++++++++++++++++++++++++++++++ \n");
   printf("\n\t\t EL NUMERO INTRODUCIDO TIENE MAS DE 5 DECIMALES\n");
   printf("\n\t\t\t TRUNCO LOS RESTANTES DECIMALES\n");
   printf("\n\t\t ++++++++++++++++++++++++++++++++++++++++++++++++ \n");
  }
  else   /*si el número de dígitos en la parte decimal es menor a igual a 5*/
  {
   strcat(leo,leo1);  /*concatenación: esto es para evitar errores
                        a la hora de la conversión debido al carácter de fin
                        de línea('/0')... esto será explicado en
                        el informe*/
   valor=atof(leo);  /*conversión */
  
  }
  /*IMPRIME # INTRODUCIDO*/
  printf("\n\t\tEL NUMERO INTRODUCIDO ES %.5lf\n", valor1);
  printf("\n\t\t ++++++++++++++++++++++++++++++++++++++++++++++++ \n");
  printf("\n\t\t ++++++++++++++++++++ RESULTADOS ++++++++++++++++\n");
  /* ****************************************************************/
  /*COLOCO EN LA FILA 0 DE LA MATRIZ ENTERO, EL DATO INGRESADO POR EL
  USUARIO*/
  for (i=0;i<tam;i++)
  {
    valoraux=valor*10;/*MULT POR 10 PARA TENER EL ELEMENTO EN PARTE ENTERA*/
    aux=valoraux;/*ME QUEDO CON LA PARTE ENTERA DEL RESULTADO ANTERIOR*/
    valor=valoraux-aux; /*LE RESTO LA PARTE ENTERA DEL RESULTADO ANTERIOR*/
    entero[0][i]=aux;/*ASIGNO A LA MATRIZ EN LA FILA 0*/
    
   }
   /***************************************************************/
   /*PROCEDIMIENTO DE CONVERSIÓN*/

   k=0, salir=0, iguales=0; /*INICIALIZACIÓN*/
   for (j=0;j<300;j++)  /*PARA OBTENER 300 DÍGITOS DECIMALES PARA EL # BINARIO*/
   {

     if (salir)  /*SALIMOS SI SALIR ES DIFERENTE DE CERO*/
     {
      printf("\n\t\t ** EXACTA** \n");/*este caso corresponde a una
                                       solución exacta*/
      break; /*se rompe el ciclo*/
     }  /*end if*/

     /*INICIALIZACIÓN*/
     sumo=0; /*tendra valores de 1 o cero*/ /**/
     for (i=4;i>=0;i--)
     /*empezamos las operaciones por el último elem de la fila*/
     {
      aux=(entero[j][i])*2+sumo;  /*mult por 2 y sumo lo que quedo de la
                                    operación anterior*/
         /*si aux es mayor o igual que 10 le resto 10 y me queda 1 que coloco
         en sumo*/
      if (aux>=10)
      {
       aux=aux-10; /*como estamos multiplicando por dos y el valor de cada
       columna esta entre 0 y 9, la mult por dos no pasara de 19*/
       sumo=1;
      }  /*end if*/
      else sumo=0;  /*si es menor estricto que 10, no queda nada*/

      entero[j+1][i]=aux; /*asigno valor, note que esta entre 0 y 9*/

      if(i==0) /*primer elemento de la fila*/
      {
       bin[k]=sumo; /*vector con los valores de la transformación a base dos,
       sumo con 0 ó 1, depende de el número obtenido en la operación*/
       k=k+1; /*me muevo a la sig casilla del vector*/
      } /*end if*/

     }/*end for i*/

   /*********************************************************************/
     /*for para verificar si se repite el vector enteror[j fijo][i]*/

     for (h=0;h<j+1; h++)  /*va de h=0 a j+1 (j del primer ciclo)*/
     {
       for (i=0;i<5;i++)
       {
        if (entero[j+1][i]!=entero[h][i])
        {
         iguales=0;  /*si algún elemnto es diferente, esto indica que ya no
         son iguales y se sale del for i*/
         break;
        }   /*end if*/
        else iguales=1; /*si no se rompe el ciclo significa que son iguales*/
       }  /*end for i*/

        /*PERIODO EN CASO DE QUE CIERTA CANTIDAD DE CIFRAS SE REPITA*/

       if (iguales==1)
       {
        printf("\n\t\t** NUMERO PERIODICO **\n");
        printf("\n\t\tPERIODO: \n");
        printf("\t\t\t");
        for (per=h;per<j+1;per++) /*VA DESDE LA FILA DONDE SE ENCONTRO LA
        REPETICIÓN, HASTA EL VALOR ACTUAL + 1, ES DECIR, J+1*/
        printf("%d",bin[per]);
        break;
       }
     } /*END FOR H*/


      if (iguales==1) break; /*SALGO DEL CICLO J*/


    /********************************************************************/
      /*FOR PARA VERIFICAR SI ES EXACTA*/
     for (i=0;i<5;i++)
     {
      if(entero[j+1][i]!=0)   /*SI HAY ALGUNO QUE NO SEA IGUAL A CERO NO ES
                                EXACTA*/
      {
        salir=0; /*NO ES EXACTA*/
        break;
      }   /*END IF*/
      else
       salir=1; /*ES EXACTA*/
     }  /*END FOR I*/
   }  /*END FOR J*/
  /*******************************************************************/
    /*INFINITO*/
   if(j==300 && salir==0)printf("\n\t\t ** INFINITO **\n");
    /*imprime solución*/
   printf("\n\tNUMERO BINARIO=> 0.");   /*impresión de los resultados*/
   for (i=0;i<k;i++) printf("%d",bin[i]);  
  /*******************************************************************/
  printf("\n\n\n\tPRESIONE UNA TECLA PARA CONTINUAR\n");
  getch(); //ESPECIE DE PAUSA

 }/*end while infinito que sale cuando el usuario tecclea un num dif de 1*/

 printf("\n\t\t**** FIN DE PROGRAMA *** ");
 getch();  /*enter para finalizar*/
 return(0);  /*retorna cero*/
}