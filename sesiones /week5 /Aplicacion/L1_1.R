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
paquetes_set <- c("readstata13", "dplyr", "tidyverse","sjlabelled",
                  "survey", "stargazer", "caret", "foreign","readr")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

source(paste0(ruta,"/","Funciones_EEA.R"))

#============================================================
# Import datasets ---------------------------------
#============================================================

for (i in 2016:2018) {
  # dataset value-added
  #===========================================================================
  data_va              <- read.dta13(paste0(ruta,"/",out,"/","a",i,"_s11_fD2_c03_1.dta"))
  # base valor agregado
  base_rva             <- funcion_rva(data_va)
  base_rva             <- base_rva %>% mutate(ryear=as.numeric(i))
  #saveold data
  saveRDS(base_rva     , file = paste0(ruta,"/",out,"/","frame_rva_",i,".RDS"))
  
  # dataset labor
  #===========================================================================
  data_labor           <- read.dta13(paste0(ruta,"/",out,"/","a",i,"_s11_fD2_c11_1.dta"))
  # base valor trabajo
  base_rlabor          <- funcion_rlabor(data_labor)
  base_rlabor          <- base_rlabor %>% mutate(ryear=as.numeric(i))
  saveRDS(base_rlabor   , file = paste0(ruta,"/",out,"/","frame_rlabor_",i,".RDS"))
  
  # dataset capital
  #===========================================================================
  data_capital         <- read.dta13(paste0(ruta,"/",out,"/","a",i,"_s11_fD2_c05_1.dta"))
  # base valor capital
  base_rcapital        <- funcion_rcapital(data_capital)
  base_rcapital        <- base_rcapital %>% mutate(ryear=as.numeric(i))  
  saveRDS(base_rcapital , file = paste0(ruta,"/",out,"/","frame_rcapital_",i,".RDS"))
  
  }
# rm(base_rva, base_rlabor, base_rcapital, data_va, data_labor, data_capital)
rm(data_va, data_labor, data_capital)   # borrar informacion anterior de data

## read datasets ------------------------------------------------------
temp      <- list.files(path = paste0(ruta,"/",out,"/"), pattern = "frame_rva_")
setwd(paste0(ruta,"/",out,"/"))
rds       <- lapply(temp, readRDS)
frame_rva <- do.call(rbind, rds)

temp      <- list.files(path = paste0(ruta,"/",out), pattern = "frame_rcapital_")
rds <- lapply(temp, readRDS)
frame_rcapital <- do.call(rbind, rds)

temp      <- list.files(path = paste0(ruta,"/",out), pattern = "frame_rlabor_")
rds <- lapply(temp, readRDS)
frame_rlabor <- do.call(rbind, rds)

frame_rva       %>%  head()
frame_rcapital  %>%  head()
frame_rlabor    %>%  head()

rm(base_rva, base_rlabor, base_rcapital)   # borrar informacion anual

## (Merge) Union de datasets -------------------------------------------

bases_empresas <- merge(frame_rva     , frame_rlabor  , by = c("iruc","ryear"))
bases_empresas <- merge(bases_empresas, frame_rcapital, by = c("iruc","ryear"))

rm(frame_rva, frame_rlabor, frame_rcapital)   # borrar informacion anual

bases_empresas %>%  names()
bases_empresas %>%  head()

# Limpieza de valores -------------
Hmisc::describe(bases_empresas$ryear)
summary(bases_empresas[,c("rva","rl","rinsumo","rk")])


# deflactores ---------------
bases_empresas$sector1 <- 9
deflactor <- read.dta13(paste0(ruta,"/",out,"/","BD_Deflactores.dta"))
# ?subset
deflactor %>%  names()
bases_empresas <- merge(bases_empresas, deflactor, by = c("sector1","ryear"))
bases_empresas %>%  summary()

bases_empresas <- bases_empresas %>% 
  filter(rva>0 & rk>0 & rinv>0 & rl>0 & rinsumo>1)
bases_empresas[,c("rva","rk","rinv","rl","rinsumo")] %>%  summary()

# Base y variables finales --------------
bases_final <- bases_empresas %>% 
  mutate(Y = 100*rva / d_VA,
         K = 100*rk / d_FBK_Fijo,
         L = rl,
         insumos = rinsumo,
         INV = rinv,
         lnY = log(Y),
         lnK = log(K),
         lnL = log(L),
         lnI = log(insumos),
         lnINV = log(INV),
         idempresa = as.numeric(iruc),
         ) %>%  
  dplyr::select(iruc, idempresa, ryear, Y,K,L, insumos, INV,
                lnY, lnK, lnL, lnI, lnINV)

bases_final %>%  dim()
bases_final %>%  head()
Hmisc::describe(bases_final$ryear)

bases_final %>%  names()
bases_final %>% head()

bases_final$iruc     <- set_label(bases_final$iruc    , c("Codigo empresa") )
bases_final$Y        <- set_label(bases_final$Y       , c("Valor agregado") )
bases_final$K        <- set_label(bases_final$K       , c("Valor capital") )
bases_final$INV      <- set_label(bases_final$INV     , c("Valor inversion") )
bases_final$insumos  <- set_label(bases_final$insumos , c("Valor insumos") )


## guardar la informacion
saveRDS(bases_final, file= paste0(ruta,"/",out,"/","BD_EEA_206_2018.RDS"))
