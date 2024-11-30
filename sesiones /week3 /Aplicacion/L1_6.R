rm(list = ls())
setwd("C:\\Users\\et396\\Dropbox\\Docencia\\UNI\\L3\\Aplicacion\\clean")

paquetes_set <- c("readstata13", "dplyr", "tidyverse", "Hmisc",
                  "survey", "stargazer", "texreg", "sjPlot", 
                  "ggplot2", "caTools","caret","MLmetrics","boot",
                  "MASS", "AER","pscl","rio","arrow","fst")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=T)

# library(rio)
# library(dplyr)
# #install.packages("MASS")
# library(MASS)
# #install.packages("pscl")
# library(pscl)
# #install.packages("AER")
# library(AER)


conteo <- import ("BD_Conteo_2021.dta")

# Lista de Variables
conteo %>%  head()
conteo <- conteo %>% 
  mutate(rriqueza=factor(rriqueza, levels=0:4),
         rregion=factor(rregion,levels=0:2))

#Hmisc::describe(conteo$rn_hijos)
table(conteo$rn_hijos)
table(conteo$rregion)
table(conteo$rriqueza)

conteo %>%  str()
summary(conteo$rn_hijos)
#Hmisc::describe(conteo$rn_hijos)

#### MODELO DE POISSON
#------------------------------------------------
m1_p <- glm(rn_hijos ~ 1, family = "poisson", data = conteo)
summary(m1_p)
conteo$pred_p1 <- predict(m1_p,newdata = conteo, type="response")

# Test de overdispersion
dispersiontest(m1_p)
plot(conteo$rn_hijos, conteo$rn_hijos, type = "o" )

m2_p <- glm(rn_hijos ~ reduca+rregion + rriqueza, family = "poisson", data = conteo)
summary(m2_p)
conteo$pred_p2 <- predict(m2_p,newdata = conteo, type="response")
summary(conteo$pred_p2)

# Test de overdispersion
dispersiontest(m2_p)

# Make tex file
tabla1 <-  texreg(m2_p, label = "tab:1",
                  #custom.coef.names = c(),
                  custom.model.names = c("OLS model"),
                  caption = "Model OLS",
                  float.pos = "h", 
                  return.string = TRUE, 
                  bold = 0.05, 
                  stars = c(0.01, 0.05, 0.1),
                  #custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, 
                  leading.zero = FALSE, 
                  omit.coef = "Inter")
tabla1
# export to latex
write(tabla1, file = "tabla_m1.tex")


### MODELO BINOMIAL NEGATIVO
m3_b <- glm.nb(rn_hijos ~ reduca+rregion + rriqueza, data = conteo)
summary(m3_b)


### MODELO ZERO-INFLATED
m4_b <- zeroinfl(rn_hijos ~ reduca+rregion + rriqueza,link="logit",
                 dist="poisson", data = conteo)
summary(m4_b)


# Make tex file
tabla2 <-  texreg(list(m2_p, m3_b, m4_b), label = "tab:1",
                  #custom.coef.names = c(),
                  custom.model.names = c("Poisson", "BN", "Zero"),
                  caption = "Model Conteo",
                  float.pos = "h", 
                  return.string = TRUE, 
                  bold = 0.05, 
                  stars = c(0.01, 0.05, 0.1),
                  #custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, 
                  leading.zero = FALSE, 
                  omit.coef = "Inter")
tabla2
# export to latex
write(tabla2, file = "tabla_m2.tex")
