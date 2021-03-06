/* LA MACRO RANDOMSELECT REALIZA UN MÉTODO STEPWISE */

%macro randomselect(data=, listclass=, vardepen=, modelo=, criterio=,
					sinicio=, sfinal=, fracciontrain=, directorio=&directorio); 
	options nocenter linesize=256;
	proc printto print="&directorio\kk.txt"; run;
	data _null_; file "&directorio\cosa.txt" linesize=2000; run;

	%do semilla=&sinicio %to &sfinal;
		proc surveyselect data=&data rate=&fracciontrain out=sal1234 seed=&semilla; run;
		ods output    SelectionSummary=modelos;
		ods output    SelectedEffects=efectos;
		ods output    Glmselect.SelectedModel.FitStatistics=ajuste;

		proc glmselect data=sal1234  plots=all seed=&semilla;
		class &listclass; 
		model &vardepen= &modelo/ selection=stepwise(select=&criterio choose=&criterio) details=all stats=all;
		run;   

		ods graphics off;   
		ods html close;   
		data union; i=5; set efectos; set ajuste point=i; run;
		data _null_; semilla=&semilla; file "&directorio\cosa.txt" mod linesize=2000; set union; put effects; run;
	%end;

	proc printto; run;
	data todos;
	infile "&directorio\cosa.txt" linesize=2000;
	length efecto $ 1000;
	input efecto @@;
	if efecto ne 'Intercept' then output;
	run;

	proc freq data=todos; tables efecto / out=sal; run;
	proc sort data=sal; by descending count;
	proc print data=sal; run;

	data todos;
	infile "&directorio\cosa.txt" linesize=2000;
	length efecto $ 1000;
	input efecto $ &&;
	run;

	proc freq data=todos; tables efecto / out=salefec; run;
	proc sort data=salefec; by descending count;
	proc print data=salefec; run;
	data _null_; set salefec; put efecto; run;
%mend;


/************************************************************************************/
/* MACRO VALIDACIÓN CRUZADA PARA REGRESIÓN NORMAL */


%macro cruzada(archivo=, /* archivo de datos */
			   vardepen=, /* nombre de la variable dependiente */
			   conti=, /* variables independientes continuas */
			   categor=, /* variables independientes categorica */
			   ngrupos=, /* número de grupos a dividir por validación cruzada */
			   sinicio=, /* semilla de inicio */
			   sfinal=); /* semilla de inicio */
	
	/* Archivo temporal SAS*/
	data final; run;

	%do semilla=&sinicio %to &sfinal;
		/* Hacemos particion de datos */
		data dos; set &archivo; u=ranuni(&semilla); /* creamos la variable semilla */
		proc sort data=dos; by u; run; /* ordenamos por la variable semilla */
		data dos (drop=nume); retain grupo 1; set dos nobs=nume; if _n_ > grupo*nume / &ngrupos then grupo=grupo+1;
		run;
		/****/

		data fantasma; run;

		%do exclu=1 %to &ngrupos;
			data tres; set dos; if grupo ne &exclu then vardep=&vardepen;

			/* procedimiento stepwise*/
			proc glm data=tres noprint;
				/* diferenciando entre variables cat o cont*/
				%if &categor ne %then %do; class &categor; model vardep=&conti &categor; %end;
				%else %do; model vardep=&conti; %end;
				/* salida resultados */
				output out=sal p=predi;
			run;
			
			/* resultados */
			data sal; set sal;
				/**/
				resi2=(&vardepen-predi)**2;
				if grupo=&exclu then output;
			run;

			data fantasma; set fantasma sal; run;
		%end;


		proc means data=fantasma sum noprint;
			var resi2;
			output out=sumaresi sum=suma mean=media;
		run;

		data sumaresi;
			set sumaresi;
			semilla=&semilla;

		data final (keep=suma media semilla);
			set final sumaresi;
			if suma=. then delete;
		run;
	%end;

	proc print data=final; run;

%mend;







/*********************************************************************************************/
/* MACRO VALIDACIÓN CRUZADA PARA REDES NEURONALES */

%macro cruzadaneural(archivo=, /* archivo de datos */
					 vardepen=, /* nombre de la variable dependiente */
					 conti=, /* variables independientes continuas */
					 categor=, /* variables independientes categorica */
					 ngrupos=, /* número de grupos a dividir por validación cruzada */
					 sinicio=, /* semilla de inicio */
					 sfinal=, /* semilla de fin */
					 nodos=3, /* número de nodos */
					 algo=levmar, /*algoritmo (poner bprop mom=0.2 learn=0.1 si es bprop) */
					 acti=tanh, /* función de activación */
				     early=, /* iteraciones early stopping (dejar como early=, si no se desea) */
					 directorio=); /* directorio para destinar datos basura */

	/*Si no se quiere información en output usar esto (cambiar el archivo de destino):
	proc printto print='&directorio\basura.txt'; */
	proc printto print="&directorio\basura.txt"; 

	data final; run;

	%do semilla=&sinicio %to &sfinal;
		data dos;
			set &archivo;
			u=ranuni(&semilla);

		proc sort data=dos;
			by u;
		run;

		data dos (drop=nume);
			retain grupo 1;
		 	set dos nobs=nume;
		 	if _n_ > grupo*nume / &ngrupos then grupo=grupo+1;
		run;

		data fantasma;
		run;

		%do exclu=1 %to &ngrupos;
			data trestr tresval;
				set dos;
				if grupo ne &exclu then output trestr;
				else output tresval;

			PROC DMDB DATA=trestr dmdbcat=catatres;
				target &vardepen;
				var &vardepen &conti;
				%if &categor ne %then %do;
					class &categor;
				%end;
			run;

			proc neural data=trestr dmdbcat=catatres random=789 ;
			input &conti;
			%if &categor ne %then %do;
				input &categor / level=nominal;
			%end;
			target &vardepen;
			hidden &nodos /	act=&acti;/*<<<<<******PARA DATOS LINEALES ACT=LIN (funciónde activación lineal) 
			NORMALMENTE PARA DATOS NO LINEALES MEJOR ACT=TANH */
			/* A PARTIR DE AQUI SON ESPECIFICACIONES DE LA RED, SE PUEDEN CAMBIAR O AÑADIR COMO PARÁMETROS */

			/* ESTO ES PARA EARLY STOPPING (maxiter=numero de iteraciones limitado)*/
			%if &early ne %then %do;
				nloptions maxiter=&early;
				netoptions randist=normal ranscale=0.1 random=15115;
			%end;

			/* %else %do;prelim 10;%end;*/
			%if &early ne %then %do;
				train maxiter=&early /* early stopping cambiar maxiter=25 por ejemplo */ outest=mlpest technique=&algo;
			%end;
			%else %do;
				train maxiter=100 outest=mlpest technique=&algo /* bprop mom=0.2 learn=0.1*/;
			%end;

			score data=tresval role=valid out=sal;
			run;

			data sal;
				set sal;
				resi2=(p_&vardepen-&vardepen)**2;
			run;

			data fantasma;
				set fantasma sal;
			run;
		%end;

		proc means data=fantasma sum noprint;
			var resi2;
			output out=sumaresi sum=suma mean=media;
		run;

		data sumaresi;
			set sumaresi;
			semilla=&semilla;

		data final (keep=suma media semilla);
			set final sumaresi;
			if suma=. then delete;
		run;
	%end;

	proc printto;
	run;

	proc print data=final;
	run;
%mend;


%macro redneuronal(archivo=,listclass=,listconti=,vardep=,porcen=,semilla=,ocultos=,algo=,acti=);

	/* Vamos a crear el catálogo con las variables */
	/* Si la lista de variables de clase esta vacía */
	%if &listclass eq %then %do;
		PROC DMDB DATA=&archivo dmdbcat=catauno;
			target &vardep;
			var &listconti &vardep;
		run;
	%end;
	%else %do;
		PROC DMDB DATA=&archivo dmdbcat=catauno;
			target &vardep;
			var &listconti &vardep;
			class &listclass;
		run;
	%end;

	/* Guardamos una tabla con los datos */
	data ooo;
		set &archivo;
	run;

	/* Hacemos la particion de los datos */
	data datos;
		set ooo nobs=nume;
		tr=int(&porcen*nume);
		call symput('tr', left(tr));
		u=ranuni(&semilla);
	run;

	proc sort data=datos;
		by u;
	run;

	data datos valida;
		set datos;
		if _n_ > tr then output valida;
		else output datos;
	run;
	/****/

	/* Creamos el modelo */
	proc neural data=datos dmdbcat=catauno validata=valida graph;
		input &listconti / id=i;
		input &listclass / level=nominal;
		target &vardep / id=o;
		hidden &ocultos / id=h act=&acti;
		nloptions maxiter=10000;
		netoptions randist=normal ranscale=0.1 random=15115;
		train maxiter=10000 outest=mlpest estiter=1 technique=&algo;
		score data=datos out=mlpout outfit=mlpfit;
		score data=valida out=mlpout2 outfit=mlpfit2 role=valid;
	run;


	data mlpest2;
		k=3;
		retain iterepocas 0;
		set mlpest;
		eval=_VOBJERR_;
		x3=lag3(eval);
		x6=lag6(eval);
		if _n_ > 6 and eval > x3 and eval > x6 then iterepocas=_n_;
	run;

	data;
		set mlpest2 nobs=nume;
		if iterepocas ne 0 then do;
			control=1;
			call symput('earlystop',left(iterepocas));
			stop;
		end;
		if _n_ = nume and control ne 1 then do;
			ka=0;
			call symput('earlystop',left(ka));
			end;
		run;


	data fin;
		j=&earlystop;
		set mlpest point=j;
		output;
		stop;
	run;

	data mlpest;
		set mlpest nobs=nume; 
		if _n_=&earlystop then do;
			cosa1=put(_OBJERR_,20.6) ;
			cosa2=put(_VOBJERR_,20.6) ;
		end;
		else do;
			cosa1=' ';
			cosa2=' ';
		end;
	run;


	title1 
	h=2 box=1 j=c c=red 'TRAIN' c=blue '  VALIDA' 
	h=1.5 j=c c=black "EARLY STOPPING=&earlystop " "semilla=&semilla" 
	h=1 j=c c=green "NODOS OCULTOS: &ocultos  " " METODO: &algo "  "ACTIVACIÓN: &acti";
	;

	symbol1 c=red v=circle i=join pointlabel=("#cosa1" h=1 c=red position=bottom  j=c);
	symbol2 c=blue v=circle i=join pointlabel=("#cosa2" h=1 c=blue position=top j=c);

	axis1 label=none;
	proc gplot data=mlpest;plot _OBJERR_ *_iter_=1 _VOBJERR_*_iter_=2 
	/overlay href=&earlystop vaxis=axis1 haxis=axis1;
	run;

	proc print data=fin;
		var  _iter_ _OBJERR_  _AVERR_  _VNOBJ_   _VOBJ_  _VOBJERR_  _VAVERR_;
	run;

%mend;

