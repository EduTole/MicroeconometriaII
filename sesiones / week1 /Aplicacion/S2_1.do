*********************************************
*
* Institucion:			UNI
* Autor:				Edinson Tolentino
* Proyecto:				Efectos marginales
* Fecha: 				Novimebre	- 2023
*********************************************
	cls
	clear all
	*--------------------------------------------------
	*Paso 1: Direccion de carpeta
	*--------------------------------------------------
	*Dirección de carpeta de bases ENAHO
	* C:\Users\et396\Dropbox\Docencia\UNI\L1\Aplicacion
	glo path "C:\Users\et396\Dropbox" // ET
	glo main "${path}/Docencia/UNI/L1/Aplicacion"
	glo data "${main}/clean"

*******************************************************
	cls
	u "${data}/BDindependiente.dta",clear
	d

	glo Xs "rneduca1-rneduca4 rnojobs redad redadsq rpobre"
	glo Zs "rneduca2-rneduca4 rnojobs redad redadsq rpobre"
	glo Ws "i.reduca_niv i.rnojobs c.redad i.rpobre"
	
	/// Var. Dependiente
	tab rnegocio
	
	/// Estadisricas
	sum rnegocio $Xs
	
	/// Estimaciones
	reg 	rnegocio $Zs
	logit 	rnegocio $Zs
	probit 	rnegocio $Zs
	
	*** Efectos Marginales (EF)
		logit rnegocio $Ws

		** EF Promedio (EMP)
		margins, dydx(rpobre)
		margins, dydx(*)

		** EF en la Media (EMM)
		margins, dydx(*) atmeans

		** EF evaluados en valores relevantes (EMEVR)
		/// Persona pobre, con nivel educativo secuendaria y edad igual a 30
		margins, dydx(*) at(rpobre=0 reduca_niv=2 redad=30)

	*** Predicciones

	margins
	margins, atmeans
	
	margins reduca_niv, atmeans
	marginsplot

	*logit rpoverty i.relectricidad c.lnr6 i.rneduca c.redad
	logit rnegocio $Ws
	margins i.reduca_niv#i.rpobre
	set scheme  plotplainblind
	marginsplot

	#delimit;
	marginsplot, 
	title("Pr. que un jefe hogar caiga en condicion de independiente voluntario según nivel educativo")
	ytitle("Probabilidad") xtitle("")
	legend(rows(1) just(center) position(6))
	ylabel(0.20(0.1)0.9)
	scheme(plotplainblind);
	#delimit cr

