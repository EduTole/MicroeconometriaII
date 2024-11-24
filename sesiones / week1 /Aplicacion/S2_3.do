*********************************************
* Institucion:			
* Autor:				Edinson Tolentino
* Proyecto:				Modelo Probit Ordenado
* Fecha:				Mayo
*********************************************
	cls 
	clear all

	*--------------------------------------------------
	*Paso 1: Direccion de carpeta
	*--------------------------------------------------
	/* 
	nota: la rutas de globales que se escriben aqui pueden ser cambiadas 
	Ud. puede cambiar las lineas de programacion de 18 hasta 23 solo por 
	el comando cd "ruta de carpeta donde se podra ubicar su archivo
	Considerar borrar los texto "${}", para que pueda ejecutar sin problema
	el codigo
	*/
	
	glo clase 	"C:\Users\et396\Dropbox\Docencia\UNI\L1" // Clase 1	
	*Adress files origin 
	glo main 	"${clase}/Aplicacion/Clean"
	glo Imagen	"${clase}/Imagen"		// Imagen
	glo Tablas	"${clase}/Tablas"		// Tablas

*--------------------------------------------------
*Paso 2: Carga de data
*--------------------------------------------------

	u "${main}/BD4_Multiproducto_2021.dta",clear
	*Descripccion de variables
	d
*	g redad3=redad*redad*redad*redad*redad
	*Variables
	glo Xs "rmujer rcivil_rpareja redad redadsq reduca rmu lnrgasto rpeople"
	sum rvida $Xs  

	*Pregunta 1
	*Estimando los cortes del umbral
	oprobit rvida  
	tab rvida  

	*Pregunta 2-3
	oprobit rvida $Xs  

	*Pregunta 3
	display - _b[redad]/(2*_b[redadsq])
	
	***PLot of age/satisfaction relationship
	gen pred_y=_b[redad]*redad + _b[redadsq]*redadsq
	scatter pred_y redad
	
	*Prueba hipotesos punto Ho : 40
	nlcom - _b[redad]/(2*_b[redadsq]) -40
	
	graph export "t1.png", replace
		

	*Pregunta 4
	*eststo clear
		quietly oprobit rvida $Xs 
		eststo oprobit
		
		quietly oprobit rvida $Xs 
		margins, dydx(*) predict(outcome(1)) post
		quietly oprobit rvida $Xs 
		margins, dydx(*) predict(outcome(2)) post
		quietly oprobit rvida $Xs 
		margins, dydx(*) predict(outcome(3)) post
		quietly oprobit rvida $Xs 
		margins, dydx(*) predict(outcome(4)) post
	
		foreach o in 1 2 3 4  {
		quietly oprobit rvida $Xs 
		margins, dydx(*) predict(outcome(`o')) post
	 }		
	 
	*Pregunta 5: relacion de años de edad y gastos del hogar
	oprobit rvida $Xs

	*Extraer matrices
	matrix b=e(b) 
	mat list b
	matrix vb=e(V) 
	mat list vb
	matrix vage=vb[7..8,7..8]
	matrix list vage

	*Pregunta 6: relacion de ,miembros del hogar y gastos del hogar
	nlcom - _b[rpeople]/_b[lnrgasto]  -0
		