*********************************************
*
* Institucion:			UNI
* Autor:				Edinson Tolentino
* Proyecto:				Enaho 2021 - Heckman
* Fecha:				Mayo	
*********************************************
cls 
clear all

*--------------------------------------------------
*Paso 1: Direccion de carpeta
*--------------------------------------------------
glo main "C:\Users\et396\Dropbox\Docencia\UNI\L2"
*Adress files origen 
glo base 	"${main}//Aplicacion/clean"
glo Imagen	"${main}/Imagen"		// Imagen
glo Tablas	"${main}/Tablas"		// Tablas

*--------------------------------------------------
*Paso 2: Carga de data
*--------------------------------------------------

	u "${base}/BD3_Sesgo_2021.dta",clear

	*Covariables
	glo Zs 			"reduca rpareja redad redadsq rnh6 rnh12"
	glo Xs 			"reduca rpareja rexper rexpersq"
	glo Controls 	"reduca rpareja rexper rexpersq redad redadsq rnh6 rnh12"
	glo Vs "reduca rpareja rexper rexpersq "
	glo Co "lnr6prin r6prin reduca redad redadsq rpareja rn* rexper rexpersq rflp"


	*Pregunta 1
	sum $Co

	*Pregunta 2
	probit rflp $Zs  

	*Pregunta 2.2
	*Edad maximiza de participacion	
	display -_b[redad]/(2*_b[redadsq])
	log using "${Tablas}/resultados_3.log" , replace	
	nlcom - _b[redad]/(2*_b[redadsq]) -34
	gen pred_y=_b[redad]*redad + _b[redadsq]*redadsq
	scatter pred_y redad
	log close
	graph export "${Imagen}/t1.png", replace		
	
	*Pregunta 2.3
	*Efectos marginales
	probit rflp $Zs  
	margins , dydx(*) post
	
	probit rflp $Zs  
	mfx
	
	probit rflp $Zs  
	margins , dydx(*) atmeans

	*Pegunta 3
	probit rflp $Zs  
	*mfx
	
	predict phat /* prediccion de la regresion probit*/
	gen z=invnorm(phat) /*valores del indice probit estandarizado*/	
	////The Inverse Mills
	gen den_z=normalden(z) /*valor de densidad de probabilidad */
	gen sct=den_z/phat /*Inversa de ratio de Mills*/	
	sum sct 
	sum sct if rflp==1
	
	*Resumen residuos
	gen resids=((rflp-phat)/(phat*(1-phat)))*normalden(z) 
	su resids
	su resids if rflp==1
	
	reg lnr6prin $Vs 		if	rflp==1	,r
	display -_b[rexper]/(2*_b[rexpersq])
	
	reg lnr6prin $Vs sct 	if 	rflp==1	,r		  
	reg lnr6prin $Vs resids if 	rflp==1 ,r			  

	*Pregunta 4
	*Regression sin variable seleccion 
	reg lnr6prin $Vs sct if rflp==1	,r		
	
	*Calculando el sesgo de selccion
	sum sct if rflp==1
	local p1=r(mean)
	display _b[sct]*`p1'
	
	*Pregunta 5
	reg lnr6prin $Controls , r
	
	*Regression basado en dos pasos a traves del procedimiento de heckman (MVC)
	*heckman lnypm $Vs , twostep select ($Zs) mills (mills)	
	heckman lnr6prin $Vs , twostep select ($Zs) mills (mills)	
		