rm(list = ls())
# rutas de librerias
#----------------------------------------------------------------
main <- "C:/Users/et396/Dropbox"
Data <- "Docencia/UNI/L1/Aplicacion/Clean"
setwd(paste0(main,"/",Data))

# Librerias
#----------------------------------------------------------------
# library(rio)
# library(dplyr)
# library(tidyverse)

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx", "mfx",
                  "stargazer", "texreg", "sjPlot", "ggplot2",
                  "readstata13","readxl")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Import dataset
#----------------------------------------------------------------
# data <- readRDS("BD_Eleccion_2021.RDS")
# data <- read_excel("BDindependiente.xlsx")
data <- read.dta13("BDindependiente.dta")

data %>%  names()       # names of variables
data %>%  str()         # structure data

# Estadisticas descriptiva
Hmisc::describe(data$rnegocio)
Hmisc::describe(data$reduca_niv)

# Modelo No lineales
formula <- "rnegocio  ~ as.factor(reduca_niv)  + rnojobs + rpobre + redad + redadsq"

# Modelo de probabilidad lineal (MPL)
m0 <- lm(formula,
         data=data)
# m0 <- lm(rnegocio  ~ as.factor(reduca_niv)  + rnojobs + rpobre + redad + redadsq ,
#          data=data)
summary(m0)

# Presentacion 1
tabla_1   <-  texreg(list(m0), label = "tab:1",
                     #custom.coef.names = c(),
                     custom.model.names = c("MPL model"),
                     caption = "MPL",
                     float.pos = "h",
                     return.string = TRUE,
                     bold = 0.05,
                     stars = c(0.01, 0.05, 0.1),
                     custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                     digits = 3,
                     leading.zero = FALSE,
                     omit.coef = "Inter")
tabla_1
# export to latex
write(tabla_1, "model_mpl.tex")

# modelo no lineal
m1 <- glm(formula ,
          family=binomial(link = "logit"), data=data)
m2 <- glm(formula,
          family="binomial", data=data)
# output 
summary(m1)
stargazer(m1)
stargazer(m1, m2,type="html",
          dep.var.labels = c("Indice Standarizado"),
          covariate.labels = c("Tipo educacion (Secundaria=1)",
                               "Tipo educacion (Tecnica=1)",
                               "Tipo educacion (Superior=1)",
                               "No encontro trabajo (=1)",
                               "Pobre (=1)",
                               "Edad (years)",
                               "Edad potencial (years square)"),
          out = "tabla_2.htm"
          )

# efectos marginales 
t1 <- probitmfx( formula ,
                            data)
t2 <- probitmfx( formula ,
                 data, atmean=FALSE)
t3 <- logitmfx(  formula ,
               data=data, atmean=FALSE)
# stargazer(t1$mfxest,t2$mfxest, type = "html",
#           out = "model_nolineal_margins.htm")

# Generar table en latex
# Preparar la tabla
modelss = list(t1$fit, t2$fit, t3$fit)
coefs   = list(c(0, t1$mfxest[, 1]), c(0, t2$mfxest[, 1]) , c(0,t3$mfxest[, 1]) )
ses     = list(c(0, t1$mfxest[, 2]), c(0, t2$mfxest[, 2]) , c(0,t3$mfxest[, 2]) )
pvals   = list(c(0, t1$mfxest[, 4]), c(0, t2$mfxest [, 4]), c(0,t3$mfxest [, 4]))

tr3 = texreg(modelss,
             override.coef = coefs,
             override.se = ses,
             override.pval = pvals,
             omit.coef = "(Intercept)",
             caption.above = TRUE,
             caption = "Models Explaining Poverty Participation. Marginal
             Effects",
             digits = 2, 
             #dcolumn = TRUE,
             #custom.note = "\%stars.",
             stars = c(0.01, 0.05, 0.1),
             custom.model.names = c("Probit (all)","Probit (mean)", "Logit (mean)"),
             return.string = TRUE)
tr3
# export to latex
write(tr3, "tabla_3.tex")

tr4 = texreg(modelss,
             override.coef = coefs,
             override.se = ses,
             override.pval = pvals,
             omit.coef = "(Intercept)",
             custom.coef.names = c("Educ. Secundaria      (==1)", 
                                   "Educ. Tecnica         (==1)", 
                                   "Educ. Supperior       (==1)", 
                                   "No encontro trabajo   (==1)", 
                                   "Pobre                 (==1)", 
                                   "Edad                  (==1)",
                                   "Edad potencial        (==1)"),
             caption.above = TRUE,
             caption = "Models Explaining Poverty Participation. Marginal
             Effects",
             digits = 2, 
             #dcolumn = TRUE,
             #custom.note = "\%stars.",
             stars = c(0.01, 0.05, 0.1),
             custom.model.names = c("Probit (all)","Probit (mean)", "Logit (mean)"),
             return.string = TRUE)
tr4
# export to latex
write(tr4, "tabla_4.tex")


# Grafico de probabilidades 
# Graficos de efectos marginales
# Modelo --> m2
table(data$reduca_niv)
summary(m2)

allmean <- data.frame(rnojobs = mean(data$rnojobs,na.rm=T),
                      rpobre = mean(data$rpobre,na.rm=T),
                      redad = mean(data$redad,na.rm=T),
                      redadsq = mean(data$redadsq,na.rm=T),
                      reduca_niv = 1:4)
                    
allmean <- cbind(allmean, predict(m2, newdata=allmean, type="response",se.fit=T))

names(allmean)[names(allmean)=="fit"] = "prob"
names(allmean)[names(allmean)=="se.fit"] = "se.prob"

allmean$ll = allmean$prob - 1.96*allmean$se.prob
allmean$ul = allmean$prob + 1.96*allmean$se.prob

allmean

# Grafico 1
ggplot(allmean, aes(x=reduca_niv, y = prob)) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.1, lty=1, lwd=1, col="red") +
  geom_point(shape=18, size=3, fill="black") +
  scale_x_discrete(limits = c("elemental","secundaria","tecnica", "superior")) +
  labs(title= " Predicted probabilities", x="Educacion", y="Pr(y=1) - Independiente Involuntario", 
       caption = "add footnote here") +
  theme(plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
        axis.title = element_text(family = "sans", size=9),
        plot.caption = element_text(family = "sans", size=5))

# Grafico de probabilidades 
# Grafico 2
summary(m2)

# Presentacion de coefientes
plot_model(
  m2, 
  bpe = "mean",
  bpe.style = "dot" ,
  ci.style = c("whisker", "bar"),
  prob.inner = .4,
  prob.outer = .8
)

plot_model(
  m2,
  type = "pred",
  terms = c("redad"),
  ci.lvl = 0.95) +theme_bw() 

plot_model(
  m2,
  type = "pred",
  terms = c("redad","rpobre"),
  ci.lvl = 0.95) +theme_bw() 


plot_model(
  m2,
  type = "pred",
  terms = c("reduca_niv"),
  ci.lvl = .95) +theme_bw()

