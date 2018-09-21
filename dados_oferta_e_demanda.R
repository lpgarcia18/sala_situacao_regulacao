library(googlesheets)
library(tidyverse)
library(reshape2)
library(zoo)



regulacao_oferta_e_demanda <- gs_title("regulacao_oferta_e_demanda")
gs_ws_ls(regulacao_oferta_e_demanda)

oferta_e_demanda<-gs_read(ss = regulacao_oferta_e_demanda, ws ="OD", col_names = T)
oferta_e_demanda <- tbl_df(oferta_e_demanda)
oferta_e_demanda <- melt(oferta_e_demanda)
names(oferta_e_demanda) <- c("PROCEDIMENTO", "TIPO", "MES", "VALOR")
oferta_e_demanda$PROCEDIMENTO <- as.factor(oferta_e_demanda$PROCEDIMENTO)
oferta_e_demanda$TIPO <- as.factor(oferta_e_demanda$TIPO)
oferta_e_demanda$MES <- as.yearmon(oferta_e_demanda$MES)
Sys.sleep(1)#Setando tempo para o google sheet não reclamar