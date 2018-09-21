library(googlesheets)
library(tidyverse)



regulacao_absenteismo <- gs_title("regulacao_absenteismo")
gs_ws_ls(regulacao_absenteismo)

absenteismo_analise_procedimento<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_analise_procedimento", col_names = T)
absenteismo_analise_procedimento <- tbl_df(absenteismo_analise_procedimento)
absenteismo_analise_procedimento
Sys.sleep(1)#Setando tempo para o google sheet não reclamar

absenteismo_analise_unidade_solicitante<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_analise_unidade_solicitante", col_names = T)
absenteismo_analise_unidade_solicitante <- tbl_df(absenteismo_analise_unidade_solicitante)
absenteismo_analise_unidade_solicitante
Sys.sleep(1)#Setando tempo para o google sheet não reclamar

#absenteismo_unidade_solicitante<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_unidade_solicitante", col_names = T)
#absenteismo_unidade_solicitante <- tbl_df(absenteismo_unidade_solicitante)
#absenteismo_unidade_solicitante
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

#absenteismo_procedimento_grupo<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_procedimento_grupo", col_names = T)
#absenteismo_procedimento_grupo <- tbl_df(absenteismo_procedimento_grupo)
#absenteismo_procedimento_grupo
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar


#absenteismo_procedimento<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_procedimento", col_names = T)
#absenteismo_procedimento <- tbl_df(absenteismo_procedimento)
#absenteismo_procedimento
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar

#absenteismo_procedimento_solicitante<-gs_read(ss = regulacao_absenteismo, ws ="absenteismo_procedimento_solicitante", col_names = T)
#absenteismo_procedimento_solicitante <- tbl_df(absenteismo_procedimento_solicitante)
#absenteismo_procedimento_solicitante
#Sys.sleep(3)#Setando tempo para o google sheet não reclamar



