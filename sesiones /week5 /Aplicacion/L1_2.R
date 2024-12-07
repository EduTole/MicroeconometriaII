###################################################
# Autor: Edinson Tolentino
# Proyecto : EEA
###################################################
rm(list=ls())


# Carpetas de informacion --------------------------
ruta   <- "C:/Users/et396/Dropbox/Docencia/UNI/L5/Aplicacion"
# base   <- "/BASES/ENAHO"
# codigo <- "/Scripts"
out    <- "/clean"

# Importar librerias -------------------------------
# Paquetes para instalar y poder usarlos
paquetes_set <- c("readstata13", "dplyr", "tidyverse",
                  "survey", "stargazer", "caret", "foreign",
                  "plm","estprod","prodest","texreg","xtable")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Carga de data de empresas -------------
data_empresas             <- readRDS(paste0(ruta,"/",out,"/","BD_EEA_206_2018.RDS"))

# Modelos --------------------------------
data_empresas %>% dim()
data_empresas %>% summary()
data_empresas %>% tail()


## MCO -----------------------------------
fm <- lnY ~ lnL + lnK
m1 <- glm(fm, data=data_empresas )
summary(m1)

## EF  -----------------------------------
# library(plm)
m2 <- plm(fm, data=data_empresas,
          index=c("iruc", "ryear"),
          model="within")
summary(m2)

## OP   ---------------------------
attach(data_empresas)
panel_sorted <- data_empresas[order(iruc, ryear), ]
m3 <- olley_pakes(data = panel_sorted,
                  lnY ~ lnL | lnK | lnI,
                  exit = NULL, 
                  id = "idempresa", 
                  time = "ryear", 
                  bootstrap = TRUE)
m3

m3a <- prodestOP(data_empresas$lnY,
                 fX = data_empresas$lnL,
                 sX = data_empresas$lnK,
                 pX = data_empresas$lnI,
                 idvar = data_empresas$idempresa,
                 timevar = data_empresas$ryear, 
                 R=5
                 )

m3a

## Levinsohn ---------------------------------------------------
attach(data_empresas)
panel_sorted <- data_empresas[order(iruc, ryear), ]
m4 <- levinsohn_petrin(data = panel_sorted, lnY ~ lnL | lnK | lnI, 
                 exit = NULL, id = "idempresa", time = "ryear", 
                 bootstrap = TRUE)
m4

m4a <- prodestLP(data_empresas$lnY,
                  fX = data_empresas$lnL,
                  sX = data_empresas$lnK,
                  pX = data_empresas$lnI,
                  idvar = data_empresas$idempresa,
                  timevar = data_empresas$ryear, 
                  R=5
)
m4a


## Presentacion de resultado -----------------------------------
# Make tex file
#modelss = list(t1$fit, t2$fit, t3$fit)
# coefs   = list(c(0, m3$t0[1]), c(0, m3$t0[2]) )
# 
# tr1 = texreg(list(m1,m2),
#              override.coef = coefs,
#              # override.se = ses,
#              # override.pval = pvals,
#              omit.coef = "(Intercept)",
#              caption.above = TRUE,
#              caption = "Models Explaining Poverty Participation. Marginal
#              Effects",
#              digits = 2, 
#              #dcolumn = TRUE,
#              #custom.note = "\%stars.",
#              stars = c(0.01, 0.05, 0.1),
#              custom.model.names = c("Probit (all)","Probit (mean)", "Logit (mean)"),
#              return.string = TRUE)
# 

tabla2 <-  texreg(list(m3a,m4a), label = "tab:1",
                  #custom.coef.names = c(),
                  custom.model.names = c("OP","LP"),
                  caption = "Modelos Porductividad",
                  float.pos = "h", 
                  return.string = TRUE, 
                  #bold = 0.05, 
                  stars = c(0.01, 0.05, 0.1),
                  #custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, 
                  leading.zero = FALSE, 
                  omit.coef = "Inter")
tabla2


tabla1 <-  texreg(list(m1,m2), label = "tab:1",
                  #custom.coef.names = c(),
                  custom.model.names = c("OLS","EF"),
                  caption = "Modelos Porductividad",
                  float.pos = "h", 
                  return.string = TRUE, 
                  #bold = 0.05, 
                  stars = c(0.01, 0.05, 0.1),
                  #custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, 
                  leading.zero = FALSE, 
                  omit.coef = "Inter")
tabla1
# export to latex
write(tabla1, file = paste0(ruta,"/",out,"/","tabla_m1.tex"))

