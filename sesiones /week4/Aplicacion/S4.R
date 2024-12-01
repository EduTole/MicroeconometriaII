rm(list=ls())
path = "C:\\Users\\et396\\Dropbox\\Docencia\\UNI\\L4\\Aplicacion"
setwd(path)

# librerias
paquetes_set <- c("tidyverse", "dplyr","readstata13", "foreign",
                  "survival", "survminer","flexsurv")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)


data <- read.dta13("BD4.dta")
# data <- read.dta("BD4.dta")

data %>% colnames()
data %>% summary()
table(data$`_d`)
data <- data %>% 
  mutate(failure = ifelse(`_d`==1,2,1))

base <- as_tibble(data)

# Estado de supervivencia
s <- Surv(base$durat, base$failure)
class(s)

# Medicion de supervivencia ------
# Kaplan-Meier
survfit(s~1)
survfit(Surv(durat, failure)~1, data=base)
sfit <- survfit(Surv(durat, failure)~1, data=base)
summary(sfit)
plot(sfit)
ggsurvplot(sfit)
ggsurvplot(survfit(Surv(durat, failure)~black, data=base))

# Modelo de Exponencial
m1 <- flexsurvreg(Surv(durat, failure) ~age + married + black 
                  + drugs +alcohol + priors+ rules+tserved, data=base,
                  dist ="exp")
m1
summary(m1)

# Modelo de Weibull
m2 <- flexsurvreg(Surv(durat, failure) ~age + married + black 
                  + drugs +alcohol + priors+ rules+tserved, data=base,
                  dist ="weibull")
m2


# Modelo de Cox -----
# Si exp(coef)= 1 : no efecto
# Si exp(coef)> 1 : incrementa Hazard
# Si exp(coef)< 1 : reduce Hazard

mod_cox <- coxph(Surv(durat, failure)~age + married + black 
                 + drugs +alcohol + priors+ rules+tserved, data=base)
mod_cox
# Coeficiente exponential 
exp(mod_cox$coefficients["age"])
