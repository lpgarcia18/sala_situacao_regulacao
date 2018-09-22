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

tempo_espera<-gs_read(ss = regulacao_oferta_e_demanda, ws ="TEMPO_ESPERA", col_names = T)
tempo_espera <- tbl_df(tempo_espera)
tempo_espera <- melt(tempo_espera)
names(tempo_espera) <- c("PROCEDIMENTO", "TIPO", "VALOR")
tempo_espera$PROCEDIMENTO <- as.factor(tempo_espera$PROCEDIMENTO)
tempo_espera$TIPO <- as.factor(tempo_espera$TIPO)
tempo_espera

previsao <- subset(tempo_espera, tempo_espera$TIPO == "PREVISÃO")

#Classificando os tempos de espera em adequado, alerta e inadequado
previsao$CLASSIFICACAO <- NA
for(i in 1:nrow(previsao)){
   if(previsao$VALOR[i] <= 30){
      previsao$CLASSIFICACAO[i] <- "Adequado"
   }else if(previsao$VALOR[i] > 30 & previsao$VALOR[i] <= 90){
      previsao$CLASSIFICACAO[i] <- "Alerta"
   } else{
      previsao$CLASSIFICACAO[i] <- "Inadequado"
   }
}
previsao <- tbl_df(previsao)
previsao$CLASSIFICACAO <- as.factor(previsao$CLASSIFICACAO)
previsao
Sys.sleep(1)#Setando tempo para o google sheet não reclamar