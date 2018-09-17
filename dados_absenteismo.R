library(googlesheets)
library(tidyverse)



regulacao <- gs_title("regulacao")
gs_ws_ls(regulacao)

#absenteismo_unidade_solicitante<-gs_read(ss = regulacao, ws ="absenteismo_unidade_solicitante", col_names = T)
#absenteismo_unidade_solicitante <- tbl_df(absenteismo_unidade_solicitante)
#absenteismo_unidade_solicitante
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

#absenteismo_procedimento_grupo<-gs_read(ss = regulacao, ws ="absenteismo_procedimento_grupo", col_names = T)
#absenteismo_procedimento_grupo <- tbl_df(absenteismo_procedimento_grupo)
#absenteismo_procedimento_grupo
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

absenteismo_analise_procedimento<-gs_read(ss = regulacao, ws ="absenteismo_analise_procedimento", col_names = T)
absenteismo_analise_procedimento$PROCEDIMENTO <- factor(absenteismo_analise_procedimento$PROCEDIMENTO, levels = absenteismo_analise_procedimento$PROCEDIMENTO[order(absenteismo_analise_procedimento$`Percent Falta`)])
absenteismo_analise_procedimento <- tbl_df(absenteismo_analise_procedimento)
absenteismo_analise_procedimento
Sys.sleep(1)#Setando tempo para o google sheet não reclamar

#absenteismo_procedimento<-gs_read(ss = regulacao, ws ="absenteismo_procedimento", col_names = T)
#absenteismo_procedimento <- tbl_df(absenteismo_procedimento)
#absenteismo_procedimento
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

#absenteismo_procedimento_solicitante<-gs_read(ss = regulacao, ws ="absenteismo_procedimento_solicitante", col_names = T)
#absenteismo_procedimento_solicitante <- tbl_df(absenteismo_procedimento_solicitante)
#absenteismo_procedimento_solicitante
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

absenteismo_analise_unidade_solicitante<-gs_read(ss = regulacao, ws ="absenteismo_analise_unidade_solicitante", col_names = T)
absenteismo_analise_unidade_solicitante$UNIDADE <- factor(absenteismo_analise_unidade_solicitante$UNIDADE, levels = absenteismo_analise_unidade_solicitante$UNIDADE[order(absenteismo_analise_unidade_solicitante$`2018`)])
absenteismo_analise_unidade_solicitante <- tbl_df(absenteismo_analise_unidade_solicitante)
absenteismo_analise_unidade_solicitante

