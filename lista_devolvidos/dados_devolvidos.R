library(googlesheets)
library(tidyverse)



regulacao_devolvidos <- gs_title("regulacao_devolvidos")
gs_ws_ls(regulacao_devolvidos)

devolvidos<-gs_read(ss = regulacao_devolvidos, ws ="devolvidos", col_names = T)

devolvidos <- devolvidos[,-8]
names(devolvidos)[6] <- "UNIDADE"
distrito_unidade <- read_csv("lista_devolvidos/bases/distrito_unidade.csv")
devolvidos <- merge(devolvidos, distrito_unidade, by = "UNIDADE", all = T)
devolvidos$ATENCAO <- NA
for(i in 1:nrow(devolvidos)){
   if(!is.na(devolvidos$DISTRITO[i])){
      devolvidos$ATENCAO[i]<- "Atenção Primária"
      }else if(devolvidos$UNIDADE[i] == "POLICLINICA MUNICIPAL CENTRO" | devolvidos$UNIDADE[i] == "POLICLINICA MUNICIPAL SUL"
               | devolvidos$UNIDADE[i] == "POLICLINICA MUNICIPAL NORTE"| devolvidos$UNIDADE[i] == "POLICLINICA MUNICIPAL CONTINENTE"){
      devolvidos$ATENCAO[i]<- "Atenção Especializada"   
      }
}

serie_historica <-gs_read(ss = regulacao_devolvidos, ws ="serie_historica", col_names = T)
serie_historica <- merge(serie_historica, distrito_unidade, by = "UNIDADE", all = T)
serie_historica_distritos <- serie_historica[,-1]
serie_historica_distritos <- aggregate(serie_historica_distritos[,-c(6)], by = list(serie_historica_distritos[,6]), FUN = sum)
names(serie_historica_distritos)[1] <- "DISTRITO"
serie_historica_todos_distritos <- serie_historica_distritos[,-c(1)]
serie_historica_todos_distritos$todos_distritos <- "todos_distritos"
serie_historica_todos_distritos <- aggregate(serie_historica_todos_distritos[,-c(6)], by = list(serie_historica_todos_distritos[,6]), FUN = sum)
serie_historica_todos_distritos$Group.1 <- NULL



senhas<-gs_read(ss = regulacao_devolvidos, ws ="senhas", col_names = T)
senhas

                                       