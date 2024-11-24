rm(list = ls())
setwd("C:/Users/et396/Dropbox/Docencia/UNI/L1/Aplicacion/Clean")

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS","erer")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)


#data <- import ("BD4_Multiproducto_2021.dta")       # base datos
data <- readRDS("BD4_Multiproducto_2021.rds")        # base datos
data %>%  dim()
data %>%  names()
data %>%  summary()
data %>%  str()
Hmisc::describe(data$rvida)

# Limpiamos informacion de NA
# data <- data %>% 
#   drop_na()

# Modelo Probit Ordenado
#install.packages("oglmx")
#library(oglmx)

# Forma 1
#-------------------------------------------------------
m1_0 <- oprobit.reg(rvida~1,data=data,savemodelframe = TRUE)
summary(m1_0)
m1_1 <- oprobit.reg(rvida~rmujer+ rcivil_rpareja+ redadsq + redad + reduca+rmu+
                    lnrgasto+ rpeople ,data=data)
summary(m1_1)
margins.oglmx(m1_1)

# Forma 2
#-------------------------------------------------------
m2_0 <- oglmx(rvida~1, data=data, link="probit", 
              constantMEAN = F, delta=0,threshparam=NULL)
summary(m2_0)

# Extraer coeficientes y modelo en latex
m2_oglmx     <- extract(m2_0,include.aic=TRUE)
t2_oglmx     = texreg(m2_oglmx)
t2_oglmx
write(t2_oglmx, "TR_1_rstudio.tex")

# Ordered probit with variables
m2_1 <- oglmx(rvida~rmujer+ rcivil_rpareja+ redad + redadsq + reduca+rmu+
                lnrgasto+ rpeople , data=data, link="probit", 
            constantMEAN = F, delta=0,threshparam=NULL)
summary(m2_1)

# Extraer coeficientes y modelo en latex
m3_oglmx <- extract(m2_1,include.aic=TRUE)
t2_oglmx  = texreg(m3_oglmx)
t2_oglmx
write(t2_oglmx, "TR_2_rstudio.tex")

# Marginal effects
Hmisc::describe(data$rvida)
margins.oglmx(m2_1)
mfe <- margins.oglmx(m2_1, atmeans = TRUE, outcomes = "All" )
mfe
stargazer(mfe$`muy bien`, type = "latex", 
          out="TR_4_rstudio.tex",
          digits = 2)

# Forma 3
#--------------------------------------
# Marginal effects
#--------------------------------------
#library(MASS)
fm   <- rvida ~ rmujer + rcivil_rpareja+ redad + reduca+rmu+lnrgasto+ rpeople
base <- data %>% dplyr::select(rvida, rmujer, rcivil_rpareja,
                               redadsq , redad , reduca, rmu, lnrgasto,
                               rpeople)
m3_2 <- polr(fm, data = base, Hess = FALSE, method = "probit")
summary(m3_2)
ME <- ocME(w = m3_2,rev.dum = T, digits = 3)
ME
# Modelo con la funcion polr
m3_polr <- extract(m3_2,include.aic=TRUE)
t3_polr = texreg(m3_polr)
t3_polr
write(t3_polr, "TR_5_rstudio.tex")

#Marginal effects
?stargazer
stargazer(ME$out$`ME.muy bien`, 
          type = "latex", out="TR_6_rstudio.tex")
stargazer(ME$out$ME.bien, 
          type = "latex", out="TR_7_rstudio.tex")

# Pregunta 3
#---------------------------------------------------------------------
summary(m2_1)
data$y_hat <- m2_1$coefficients['redad']*data$redad+ m2_1$coefficients['redadsq']*data$redadsq  
plot(data$redad, data$y_hat)

# Pregunta 4-5-6
# Probabilidades ; efectos marginales
# formato Tex
stargazer(ME$out$`ME.muy bien`, type = "latex", out="Tabla_polr_mb.tex",
          title = "Efectos Marginales (Prob y==muy bien)")
stargazer(ME$out$ME.bien, type = "latex", out="Tabla_polr_b.tex",
          title = "Efectos Marginales (Prob y==bien)")
stargazer(ME$out$ME.mal, type = "latex", out="Tabla_polr_m.tex",
          title = "Efectos Marginales (Prob y==mal)")
stargazer(ME$out$`ME.muy mal`, type = "latex", out="Tabla_polr_mm.tex",
          title = "Efectos Marginales (Prob y==muy mal)")

# formato HTML
stargazer(ME$out$`ME.muy bien`, type = "html", out="Tabla_polr_mb.htm",
          title = "Efectos Marginales (Prob y==muy bien)")
stargazer(ME$out$ME.bien, type = "html", out="Tabla_polr_b.htm",
          title = "Efectos Marginales (Prob y==bien)")
stargazer(ME$out$ME.mal, type = "html", out="Tabla_polr_m.htm",
          title = "Efectos Marginales (Prob y==mal)")
stargazer(ME$out$`ME.muy mal`, type = "html", out="Tabla_polr_mm.htm",
          title = "Efectos Marginales (Prob y==muy mal)")
