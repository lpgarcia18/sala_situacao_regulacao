library(googlesheets)
library(tidyverse)
library(reshape2)
library(zoo)

regulacao_oferta_e_demanda <- gs_title("regulacao_oferta_e_demanda")
gs_ws_ls(regulacao_oferta_e_demanda)


#Transformando tabela de tempo de espera
tempo_espera<-gs_read(ss = regulacao_oferta_e_demanda, ws ="TEMPO_ESPERA", col_names = T)
tempo_espera$PROCEDIMENTO <- as.factor(tempo_espera$PROCEDIMENTO)

#Classificando os tempos de espera em adequado, alerta e inadequado
tempo_espera$CLASSIFICACAO <- NA
for(i in 1:nrow(tempo_espera)){
   if(tempo_espera$PREVISAO[i] <= 30){
      tempo_espera$CLASSIFICACAO[i] <- "Adequado"
   }else if(tempo_espera$PREVISAO[i] > 30 & tempo_espera$PREVISAO[i] <= 90){
      tempo_espera$CLASSIFICACAO[i] <- "Alerta"
   } else{
      tempo_espera$CLASSIFICACAO[i] <- "Inadequado"
   }
}
tempo_espera <- tbl_df(tempo_espera)
tempo_espera$CLASSIFICACAO <- as.factor(tempo_espera$CLASSIFICACAO)
tempo_espera
Sys.sleep(1)#Setando tempo para o google sheet não reclamar

#Transformando tabela de oferta e demanda
oferta_e_demanda<-gs_read(ss = regulacao_oferta_e_demanda, ws ="OD", col_names = T)
oferta_e_demanda <- tbl_df(oferta_e_demanda)
oferta_e_demanda <- melt(oferta_e_demanda)
names(oferta_e_demanda) <- c("PROCEDIMENTO", "TIPO", "MES", "VALOR")
oferta_e_demanda$PROCEDIMENTO <- as.factor(oferta_e_demanda$PROCEDIMENTO)
oferta_e_demanda$TIPO <- as.factor(oferta_e_demanda$TIPO)
oferta_e_demanda$MES <- as.yearmon(oferta_e_demanda$MES)
Sys.sleep(1)#Setando tempo para o google sheet não reclamar

