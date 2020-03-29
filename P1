/***** Seleccion de variables *******/
/*proc printto log='null'; quit;*/
/* proc printto log=log; quit; */


/* Creamos el conjunto de datos */
libname saratoga 'C:\MASTER\LIB'; 
data saratoga; set saratoga.saratoga_train; 
run;



/*************************************************************
************* METODOS DE SELECCION DE VARIABLES **************
*************************************************************/

/* Métodos stepwise */
/** Criterio AIC **/
ods output  SelectedEffects=efectos;
proc glmselect data=saratoga;
    model price=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
                IMP_REP_livingArea REP_pctCollege REP_rooms
				TI_centralAir1 TI_centralAir2 TI_fuel1 TI_fuel2 TI_fuel3 TI_heating1 TI_heating2 TI_heating3 
                TI_newConstruction1 TI_newConstruction2 TI_waterfront1 TI_waterfront2
   	/ selection=stepwise(select=AIC choose=AIC);
;
proc print data=efectos; run;
data; set efectos; put effects; run;

/* IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir2 TI_heating2 TI_newConstruction1 TI_waterfront2 */



/** Criterio BIC **/
ods output  SelectedEffects=efectos;
proc glmselect data=saratoga;
    model price=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
                IMP_REP_livingArea REP_pctCollege REP_rooms 
				TI_centralAir1 TI_centralAir2 TI_fuel1 TI_fuel2 TI_fuel3 TI_heating1 TI_heating2 TI_heating3 
                TI_newConstruction1 TI_newConstruction2 TI_waterfront1 TI_waterfront2
   	/ selection=stepwise(select=BIC choose=BIC);
;
proc print data=efectos; run;
data; set efectos; put effects; run;

/* IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_heating2 TI_newConstruction1 TI_waterfront1 */



/* Random select */
/** Criterio AIC **/
%randomselect(data=saratoga,
			  listclass=,
			  vardepen=price,
			  modelo=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
                     IMP_REP_livingArea REP_pctCollege REP_rooms
					 TI_centralAir1 TI_centralAir2 TI_fuel1 TI_fuel2 TI_fuel3 TI_heating1 TI_heating2 TI_heating3 
                     TI_newConstruction1 TI_newConstruction2 TI_waterfront1 TI_waterfront2,
			  criterio=AIC,
			  sinicio=123456,
			  sfinal=123900,
			  fracciontrain=0.8,
			  directorio=C:\MASTER\);

/* Resultados 
   	Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_heating2 TI_newConstruction2 TI_waterfront1

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_heating2 TI_newConstruction1 TI_waterfront1

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_fuel1 TI_newConstruction2 TI_waterfront1

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir2 TI_heating2 TI_newConstruction1 TI_waterfront2
*/


/* Random select */
/** Criterio SBC **/
%randomselect(data=saratoga,
			  listclass=,
			  vardepen=price,
			  modelo=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
                     IMP_REP_livingArea REP_pctCollege REP_rooms
					 TI_centralAir1 TI_centralAir2 TI_fuel1 TI_fuel2 TI_fuel3 TI_heating1 TI_heating2 TI_heating3 
                     TI_newConstruction1 TI_newConstruction2 TI_waterfront1 TI_waterfront2,
			  criterio=SBC,
			  sinicio=123456,
			  sfinal=123457,
			  fracciontrain=0.8,
			  directorio=C:\MASTER\);

/*
	Resultados
		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_newConstruction2 TI_waterfront1

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_newConstruction1 TI_waterfront1

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_newConstruction1 TI_waterfront2

		Intercept IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir2 TI_newConstruction1 TI_waterfront2
*/




/*************************************************************
********************** MODELOS REGRESION CV ******************
*************************************************************/

/* Modelo 1 Stepwise AIC */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue 
    		   IMP_REP_livingArea REP_rooms 
    		   TI_centralAir2 TI_heating2 TI_newConstruction1 TI_waterfront2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final1; set final; modelo=1;

/* Modelo 2 Stepwise BIC */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue 
    		   IMP_REP_livingArea REP_rooms 
               TI_centralAir1 TI_heating2 TI_newConstruction1 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final2; set final; modelo=2;

/* Modelo 3 %randomselect AIC (1) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms 
               TI_centralAir1 TI_heating2 TI_newConstruction2 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final3; set final; modelo=3;

/* Modelo 4 %randomselect AIC (3) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue 
			   IMP_REP_livingArea REP_rooms 
			   TI_centralAir1 TI_fuel1 TI_newConstruction2 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final4; set final; modelo=4;

/* Modelo 5 %randomselect BIC (1) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms 
               TI_centralAir1 TI_newConstruction2 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final5; set final; modelo=5;

/* Modelo 6 %randomselect BIC (2) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms 
               TI_centralAir1 TI_newConstruction1 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final6; set final; modelo=6;

/* Modelo 7 %randomselect BIC (3) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms 
               TI_centralAir1 TI_newConstruction1 TI_waterfront2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final7; set final; modelo=7;

/* Modelo 8 %randomselect BIC (4) */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms 
               TI_centralAir2 TI_newConstruction1 TI_waterfront2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final8; set final; modelo=8;

/* Modelo 9 Incremento gradiente */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_fireplaces IMP_REP_landValue 
               IMP_REP_livingArea REP_pctCollege  
			   TI_centralAir1 TI_centralAir2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final9; set final; modelo=9;

/* Modelo 10 Regresion stepwise */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			   TI_centralAir2 TI_newConstruction1 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final10; set final; modelo=10;

/* Modelo 11 Minimos cuadrados */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue 
               IMP_REP_livingArea REP_rooms,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final11; set final; modelo=11;

/* Modelo 12 Seleccion de variables */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
               IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			   TI_centralAir2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final12; set final; modelo=12;

/* Modelo 13 intrseccion 4 */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final13; set final; modelo=13;

/* Modelo 14 intrseccion 4-3 */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea  REP_rooms  
               TI_centralAir2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final14; set final; modelo=14;

/* Modelo 15 intrseccion 4-3-2 */
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
               IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces 
               TI_centralAir2,
		 categor=,
		 ngrupos=10, sinicio=123456, sfinal=123500);
data final15; set final; modelo=15;

/* TODOS LOS MODELOS */

title j=c '1=Backward AIC, 2=Backward BIC, 3=%randomselect AIC(1), 4=AIC(3), 5=%randomselect BIC(1),' 
      j=c '6=BIC(2), 7=BIC(3), 8=BIC(4), 9=Incremento gradiente, 10=Regresion stepwise,'
	  j=c '11=Minimos cuadrados, 12=Seleccion, 13=interseccion 4, 14=interseccion 4-3, 15=interseccion 4-3-2';

data union;
	set final1 final2 final3 final4 final5 final6 final7 final8 final9 final10 final11 final12 final13 final14 final15;

proc boxplot data=union;
	plot media*modelo;
run;

/* LOS MEJORES MODELOS */

title j=c '1=Backward AIC, 2=Backward BIC, 3=%randomselect AIC(1), 4=AIC(3),' 
      j=c ' 5=%randomselect BIC(1), 6=BIC(2), 7=BIC(3), 8=BIC(4), 10=Regresion stepwise,';

data union;
	set final1 final2 final3 final4 final5 final6 final7 final8 final10;

proc boxplot data=union;
	plot media*modelo;
run;


/* CATALOGO */

PROC DMDB DATA=saratoga dmdbcat=satatogacat; 
Target price; 
var EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_fireplaces IMP_REP_landValue 
    IMP_REP_livingArea LOG_IMP_REP_age REP_pctCollege REP_rooms SQR_IMP_REP_fireplaces
    TI_centralAir1 TI_centralAir2 TI_fuel1 TI_fuel2 TI_fuel3 TI_heating1 TI_heating2 TI_heating3 
    TI_newConstruction1 TI_newConstruction2 TI_waterfront1 TI_waterfront2 price;
run;

/* GRAFICO: COMO VARIA EL ERROR EN FUNCION DEL NUMERO DE NODOS */

data union; run;
%do nodos=1 %to 10;
	proc neural data=saratoga dmdbcat=satatogacat;
		input EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
              IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			  TI_centralAir2 TI_newConstruction1 TI_waterfront1;
		target price;
		hidden &nodos;
		prelim 2 preiter=3;
		train tech=levmar;
		score data=saratoga out=salpredi outfit=salfit;
	run;
	data salfit; set salfit; nodos=&nodos; if _n_=2 then output;
	data union; set union salfit; run;
%end;

data union; set union; if _n_=1 then delete; run;
proc print data=union; run;


legend1 label=none frame;                                                                                                               
axis1 label=("nº nodos") minor=none offset=(1,1);                                                                                     
axis2 label=(angle=90 "RASE");  

title j=c 'RASE x número de nodos';
symbol i=join v=circle;
proc gplot data=union;
plot (_RASE_)*nodos / overlay legend=legend1                                                                               
                    haxis=axis1 vaxis=axis2; 
run;


%nodosvalcruza(ini=1, fin=10, increme=1,
               data=saratoga, vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
              	     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               acti=tanh)
proc print data=union;run;
proc boxplot data=union;plot media*modelo;run;








/* algo=levmar acti=TAHN nodos=3 semilla=123456 */
%redneuronal(archivo=saratoga,
			 listclass=,
			 listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
              		   IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			           TI_centralAir2 TI_newConstruction1 TI_waterfront1,
			 vardep=price, 
			 porcen=0.80,
             semilla=123456,
             ocultos=3,
			 algo=levmar,
             acti=TANH);



title j=c '5=red con levmar, 6=red con bprop' 
	  j=c '7=red con levmar con early 11, 8=red con bprop early 11' 
	  j=c '9=red con bprop early variables del stepwise'; 






proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=trureg, acti=tanh,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=bprop mom=0.9 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;




title j=c '3 nodos ocultos' 
	  j=c '1=levmar, 2=quanew, 3=congra, 4=dbldog, 5=trureg, 6=bprop'; 


data union;set final1 final2 final3 final4 final5 final6 /*final7 final8 final9 final10 final11 final12 final13 final14 final15*/;
proc boxplot data=union; plot media*modelo; run;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=trureg, acti=tanh,
			   directorio=C:\MASTER\);
data final11; set final; modelo=11;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=bprop mom=0.9 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final12; set final; modelo=12;


title j=c '4 nodos ocultos' 
	  j=c '7=levmar, 8=quanew, 9=congra, 10=dbldog, 11=trureg, 12=bprop'; 

data union; set final7 final8 final9 final10 final11 final12 /*final13 final14 final15*/;
proc boxplot data=union; plot media*modelo; run;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final13; set final; modelo=13;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final14; set final; modelo=14;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final15; set final; modelo=15;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final16; set final; modelo=16;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=trureg, acti=tanh,
			   directorio=C:\MASTER\);
data final17; set final; modelo=17;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=bprop mom=0.9 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final18; set final; modelo=18;





title j=c '5 nodos ocultos' 
	  j=c '13=levmar, 14=quanew, 15=congra, 16=dbldog, 17=trureg, 18=bprop'; 

data union; set final13 final14 final15 final16 final17 final18;
proc boxplot data=union; plot media*modelo; run;

proc printto log='null'; quit;


%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final19; set final; modelo=19;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final20; set final; modelo=20;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final21; set final; modelo=21;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final22; set final; modelo=22;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=trureg, acti=tanh,
			   directorio=C:\MASTER\);
data final23; set final; modelo=23;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=bprop mom=0.9 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final24; set final; modelo=24;


title j=c '6 nodos ocultos' 
	  j=c '19=levmar, 20=quanew, 21=congra, 22=dbldog, 23=trureg, 24=bprop'; 

data union; set final19 final20 final21 final22 final23 final24;
proc boxplot data=union; plot media*modelo; run;





data union; set final1  final2  final3  final4  final5  final6 
                final7  final8  final9  final10 final11 final12 
                final13 final14 final15 final16 final17 final18 
                final19 final20 final21 final22 final23 final24;
proc boxplot data=union;plot media*modelo;run;






%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=sof,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=log,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=arc,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=levmar, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

title j=c '          algoritmo=levmar' 
	  j=c '          1=TANH, 2=SOF, 3=LOG, 4=ARC, 5=LIN'; 

data union; set final1 final2 final3 final4 final5;
proc boxplot data=union; plot media*modelo; run;



%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=sof,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=log,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

title j=c '          algoritmo=congra' 
	  j=c '          6=TANH, 7=SOF, 8=LOG, 9=ARC, 10=LIN'; 

data union; set final6 final7 final8 final9 final10;
proc boxplot data=union; plot media*modelo; run;



/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/



%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=sof,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=log,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=arc,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=quanew, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

title j=c '          algoritmo=quanew' 
	  j=c '          1=TANH, 2=SOF, 3=LOG, 4=ARC, 5=LIN'; 

data union; set final1 final2 final3 final4 final5;
proc boxplot data=union; plot media*modelo; run;



%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=sof,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=log,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

title j=c '          algoritmo=congra' 
	  j=c '          6=TANH, 7=SOF, 8=LOG, 9=ARC, 10=LIN'; 

data union; set final6 final7 final8 final9 final10;
proc boxplot data=union; plot media*modelo; run;





%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final11; set final; modelo=11;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=sof,
			   directorio=C:\MASTER\);
data final12; set final; modelo=12;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=log,
			   directorio=C:\MASTER\);
data final13; set final; modelo=13;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=arc,
			   directorio=C:\MASTER\);
data final14; set final; modelo=14;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=4, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final15; set final; modelo=15;

title j=c '          algoritmo=dbldog' 
	  j=c '          11=TANH, 12=SOF, 13=LOG, 14=ARC, 15=LIN'; 

data union; set final11 final12 final13 final14 final15;
proc boxplot data=union; plot media*modelo; run;



/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/


%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=sof,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=log,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

title j=c '          algoritmo=congra' 
	  j=c '          1=TANH, 2=SOF, 3=LOG, 4=ARC, 5=LIN'; 

data union; set final1 final2 final3 final4 final5;
proc boxplot data=union; plot media*modelo; run;



%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=sof,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=log,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=arc,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=5, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

title j=c '          algoritmo=dbldog' 
	  j=c '          6=TANH, 7=SOF, 8=LOG, 9=ARC, 10=LIN'; 

data union; set final6 final7 final8 final9 final10;
proc boxplot data=union; plot media*modelo; run;




/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/



proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=sof,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=log,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=arc,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=levmar, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

title j=c '          algoritmo=levmar' 
	  j=c '          1=TANH, 2=SOF, 3=LOG, 4=ARC, 5=LIN'; 

data union; set final1 final2 final3 final4 final5;
proc boxplot data=union; plot media*modelo; run;




proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=sof,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=log,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=arc,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=quanew, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

title j=c '          algoritmo=quanew' 
	  j=c '          6=TANH, 7=SOF, 8=LOG, 9=ARC, 10=LIN'; 

data union; set final6 final7 final8 final9 final10;
proc boxplot data=union; plot media*modelo; run;



proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final11; set final; modelo=11;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
                     IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			         TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=sof,
			   directorio=C:\MASTER\);
data final12; set final; modelo=12;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=log,
			   directorio=C:\MASTER\);
data final13; set final; modelo=13;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=arc,
			   directorio=C:\MASTER\);
data final14; set final; modelo=14;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=123456, sfinal=123500, nodos=6, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final15; set final; modelo=15;

title j=c '          algoritmo=dbldog' 
	  j=c '          11=TANH, 12=SOF, 13=LOG, 14=ARC, 15=LIN'; 

data union; set final11 final12 final13 final14 final15;
proc boxplot data=union; plot media*modelo; run;






/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=levmar, acti=arc,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=levmar, acti=lin,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=bprop mom=0.2 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=3, early=, algo=bprop mom=0.9 learn=0.05, acti=tanh,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=quanew, acti=lin,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=bprop mom=0.9 learn=0.1, acti=tanh,
			   directorio=C:\MASTER\);
data final11; set final; modelo=11;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=4, early=, algo=bprop mom=0.1 learn=0.05, acti=tanh,
			   directorio=C:\MASTER\);
data final12; set final; modelo=12;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final13; set final; modelo=13;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final14; set final; modelo=14;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final15; set final; modelo=15;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final16; set final; modelo=16;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=bprop mom=0.2 learn=0.5, acti=tanh,
			   directorio=C:\MASTER\);
data final17; set final; modelo=17;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=5, early=, algo=bprop mom=0.2 learn=0.05, acti=tanh,
			   directorio=C:\MASTER\);
data final18; set final; modelo=18;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=levmar, acti=tanh,
			   directorio=C:\MASTER\);
data final19; set final; modelo=19;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=levmar, acti=lin,
			   directorio=C:\MASTER\);
data final20; set final; modelo=20;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=quanew, acti=lin,
			   directorio=C:\MASTER\);
data final21; set final; modelo=21;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=dbldog, acti=tanh,
			   directorio=C:\MASTER\);
data final22; set final; modelo=22;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final23; set final; modelo=23;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=bprop mom=0.2 learn=0.5, acti=tanh,
			   directorio=C:\MASTER\);
data final24; set final; modelo=24;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=332211, sfinal=332233, nodos=6, early=, algo=bprop mom=0.6 learn=0.01, acti=tanh,
			   directorio=C:\MASTER\);
data final25; set final; modelo=25;

title j=c'';
data union; set final1  final2  final3  final4  /*final5
                final6*/  final7  final8  final9  final10
				/*final11 final12*/ final13 final14 final15
				final16 /*final17 final18*/ final19 final20
                final21 final22 final23 /*final24 final25*/;
proc boxplot data=union; plot media*modelo; run;






%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=654321,
             ocultos=3, algo=congra, acti=TANH);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=987654,
             ocultos=3, algo=congra, acti=TANH);


%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=654321,
             ocultos=3, algo=congra, acti=LIN);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=987654,
             ocultos=3, algo=congra, acti=LIN);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=654321,
             ocultos=5, algo=congra, acti=ARC);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=987654,
             ocultos=5, algo=congra, acti=ARC);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=654321,
             ocultos=5, algo=dbldog, acti=LIN);

%redneuronal(archivo=saratoga,
             listclass=,
             listconti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1, 
             vardep=price,
             porcen=0.80, semilla=987654,
             ocultos=5, algo=dbldog, acti=LIN);









/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/
/**************************************************************/

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=3, early=, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=3, early=11, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=3, early=13, algo=congra, acti=tanh,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

/**************************************************************/
/**************************************************************/

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=10, sinicio=432100, sfinal=432199, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=3, early=10, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=3, early=13, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final6; set final; modelo=6;

/**************************************************************/
/**************************************************************/

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=5, early=, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final7; set final; modelo=7;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=5, early=12, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final8; set final; modelo=8;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=5, early=25, algo=congra, acti=arc,
			   directorio=C:\MASTER\);
data final9; set final; modelo=9;

/**************************************************************/
/**************************************************************/

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
              ngrupos=10, sinicio=432100, sfinal=432199, nodos=5, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final10; set final; modelo=10;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=5, early=17, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final11; set final; modelo=11;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=5, sinicio=456123, sfinal=456456, nodos=5, early=22, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final12; set final; modelo=12;

/**************************************************************/
/**************************************************************/

/* Modelo 10 Regresion stepwise */
proc printto log='null'; quit;
%cruzada(archivo=saratoga, vardepen=price,
		 conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue 
               IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces  
			   TI_centralAir2 TI_newConstruction1 TI_waterfront1,
		 categor=,
		 ngrupos=10, sinicio=432100, sfinal=432199);
data final13; set final; modelo=13;

data union; set /*final1  final2  final3*/  final4  /*final5
                final6  final7  final8  final9*/  final10
				/*final11 final12*/ final13;
proc boxplot data=union; plot media*modelo; run;





proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=EXP_REP_rooms IMP_REP_age IMP_REP_bathrooms IMP_REP_landValue IMP_REP_livingArea LOG_IMP_REP_age REP_rooms SQR_IMP_REP_fireplaces TI_centralAir2 TI_newConstruction1 TI_waterfront1,
               categor=,
               ngrupos=10, sinicio=553311, sfinal=553377, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final1; set final; modelo=1;


proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir2 TI_heating2 TI_newConstruction1 TI_waterfront2,
               categor=,
               ngrupos=10, sinicio=553311, sfinal=553377, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final2; set final; modelo=2;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir2 TI_heating2 TI_newConstruction1 TI_waterfront2,
               categor=,
               ngrupos=10, sinicio=553311, sfinal=553377, nodos=5, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final3; set final; modelo=3;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_heating2 TI_newConstruction2 TI_waterfront1,
               categor=,
               ngrupos=10, sinicio=553311, sfinal=553377, nodos=3, early=, algo=congra, acti=lin,
			   directorio=C:\MASTER\);
data final4; set final; modelo=4;

proc printto log='null'; quit;
%cruzadaneural(archivo=saratoga,
               vardepen=price,
               conti=IMP_REP_age IMP_REP_bathrooms IMP_REP_bedrooms IMP_REP_landValue IMP_REP_livingArea REP_rooms TI_centralAir1 TI_heating2 TI_newConstruction2 TI_waterfront1,
               categor=,
               ngrupos=10, sinicio=553311, sfinal=553377, nodos=5, early=, algo=dbldog, acti=lin,
			   directorio=C:\MASTER\);
data final5; set final; modelo=5;

data union; set final1 final2 final3 final4 final5;
proc boxplot data=union; plot media*modelo; run;
