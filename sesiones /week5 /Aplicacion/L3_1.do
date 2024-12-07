cls
clear all
cd "C:\Users\et396\Dropbox\Docencia\UNI\L5\Aplicacion\clean"


u "BDMincer_2021.dta",clear

/// Descripcion de variables
d 

/// Macros para agrupar las variables
glo Xs "redad redadsq rpareja rrural"

/// Datos descriptivas 
tabstat logry reduca rmujer $Xs, s(n mean p50 min max sd) col(stat) 

/// Pregunta 1
reg logry reduca , r
reg logry reduca rmujer redad redadsq rpareja , r 
reg logry reduca rmujer $Xs ,r 

/// Pregunta 2
* findit qreg2
reg logry reduca rmujer $Xs , r
qreg2 logry reduca rmujer $Xs, quantile (0.1)
qreg2 logry reduca rmujer $Xs 
qreg2 logry reduca rmujer $Xs , quantile (0.9)
	

