/*
---------------------------------------------------
Curso: Microeconometria Aplicada II
Tema: EEA
Autor: Edinson Tolentino
---------------------------------------------------
*/

cls
clear all
capture mkdir "C:\Users\et396\Dropbox\Docencia\UNI\L5\Aplicacion"
set more off
gl main 	"C:\Users\et396\Dropbox\Docencia\UNI\L5\Aplicacion"
gl clean 	"${main}/clean"
gl codigos 	"${main}/Codigos"
gl tablas 	"${main}/Tablas"

cd $clean

******************
*	Estimación   *
******************
cls
clear all
use "${clean}\BD5.dta", clear

*** MCO
reg lnY lnL lnK, vce(robust)
predict ln_PTF_OLS if e(sample), resid
replace ln_PTF_OLS = ln_PTF_OLS + _b[_cons] if e(sample) // PTF en Logaritmo
gen PTF_OLS = exp(ln_PTF_OLS) if e(sample) // PTF en niveles
outreg2 using "${tablas}\PTF.xls", ctitle(MCO) replace label keep(lnK lnL)

*** Efectos Fijos
destring iruc, replace
xtset iruc ryear

xtreg lnY lnL lnK , fe vce(robust)
predict ln_PTF_FE if e(sample), u 
replace ln_PTF_FE = ln_PTF_FE + _b[_cons] if e(sample)
gen PTF_FE = exp(ln_PTF_FE) if e(sample)
outreg2 using "${tablas}\PTF.xls", ctitle(FE) append label keep(lnK lnL)

*** Olley & Pakes (OP)

// Construimos ID de salida
gen firmid = iruc
sort firmid ryear
by firmid: gen count = _N
gen survivor = count == 3
gen has95 = 1 if ryear == 2016
sort firmid has95
by firmid: replace has95 = 1 if has95[_n-1] == 1
replace has95 = 0 if has95 == .
sort firmid ryear
by firmid: gen has_gaps = 1 if ryear[_n-1] != ryear-1 & _n != 1
sort firmid has_gaps
by firmid: replace has_gaps = 1 if has_gaps[_n-1] == 1
replace has_gaps = 0 if has_gaps == .
by firmid: generate exit = survivor == 0 & has95 == 0 & has_gaps != 1 & _n == _N
replace exit = 0 if exit == 1 & ryear == 2016

// Estimación
*findit opreg
opreg lnY, exit(exit) state(lnK) proxy(lnINV) free(lnL  ) vce(bootstrap, rep(50))
predict ln_PTF_OP if e(sample), tfp 
gen PTF_OP = exp(ln_PTF_OP) if e(sample)
outreg2 using "${tablas}\PTF.xls", ctitle(OP) append label keep(lnK lnL)

*** Levinsohn & Petrin (LP)
levpet lnY, free(lnL) proxy(lnI) capital(lnK) valueadded reps(50)
predict PTF_LP if e(sample), omega
gen ln_PTF_LP = ln(PTF_LP) if e(sample)
outreg2 using "${tablas}\PTF.xls", ctitle(LP) append label keep(lnK lnL)

acfest lnY, free(lnL) proxy(lnINV) state(lnK) robust invest
