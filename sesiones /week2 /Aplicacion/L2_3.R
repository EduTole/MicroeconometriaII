rm(list = ls())
setwd("C:/Users/et396/Dropbox/Docencia/UNI/L2/Aplicacion/clean")

# library(rio)
# library(dplyr)
# library(tidyverse)
# 
# install.packages(c("plm", "ivpack","sampleSelection"))
# library(plm)
# library(ivpack)
# library(sampleSelection)

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS", "plm",
                  "sampleSelection")
install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

data <- import ("BD3_Sesgo_2021.dta")

data %>%  dim()
data %>%  names()
data %>%  str()

# resumen estadistico
summary(data)

# Modelo MCO
mco <- lm(lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=subset(data, rflp==1))
summary(mco)

# modelo de heckman
?heckit
# comando 1
heckm <- heckit(rflp ~reduca + rpareja + redad + redadsq + rnh6 + rnh12,
                lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=data )
summary(heckm)

# modelo de seleccion
seleccion <- selection(rflp ~reduca + rpareja + redad + redadsq + rnh6 + rnh12,
                lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=data )
summary(seleccion)


# stargazer table
library(stargazer)
stargazer(mco, heckm, seleccion,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression de mujeres y sus retornos", type="text", 
          df=FALSE, digits=4)
