Sys.setlocale("LC_TIME","English_United States.1252")
options(encoding = "UTF-8")

########################################################################################### 
#pacotes 
###########################################################################################
library(shiny)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(DT)
library(plotly)
library(GGally)
library(zoo)
library(dplyr)


########################################################################################### 
#Banco de dados 
###########################################################################################
source("dados_absenteismo.R")
source("dados_oferta_e_demanda.R")


########################################################################################### 
#Funções 
###########################################################################################
#Função para formação do gráfico de gauge
#adaptada de https://stackoverflow.com/questions/24900903/how-to-draw-gauge-chart-in-r

gg.gauge <- function(dias, titulo,breaks=c(0,30,90,365)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/365)
    th.end   <- pi*(1-b/365)
    th       <- seq(th.start,th.end,length=365)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
  ajuste <- ifelse(dias > 365, 365, dias)
  
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(ajuste-1,ajuste+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/365)),y=1.1*sin(pi*(1-breaks/365)),label=paste0(breaks," dias")))+
    annotate("text",x=0,y=0,label=paste0(dias, " dias"),vjust=0,size=8,fontface="bold")+
    ggtitle(titulo)+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

########################################################################################### 
#UI
###########################################################################################
########################################################################################### 
ui <- dashboardPage(skin = "blue",
########################################################################################### 
        dashboardHeader(title = "Sala de Situação da Regulação do Sistema de Saúde", titleWidth = 550),
        ########################################################################################### 
        dashboardSidebar(
          ########################################################################################### 
          sidebarMenu(
            menuItem("Tempo de Espera",tabName = "oferta_e_demanda", icon = icon("dashboard"),
                     menuSubItem("Por procedimento", tabName = "tempo_de_espera_procedimento_dia"),
                     menuSubItem("Procedimentos-dia", tabName = "tempo_de_espera_procedimento_todos_dia"),
                     menuSubItem("Procedimentos-aguardando", tabName = "tempo_de_espera_procedimento_todos_aguardando"),
                     menuSubItem("Procedimentos-agendado", tabName = "tempo_de_espera_procedimento_todos_agendado"),
                     menuSubItem("Procedimentos-oferta", tabName = "tempo_de_espera_procedimento_todos_oferta")),
            menuItem("Oferta e Demanda",tabName = "oferta_e_demanda", icon = icon("dashboard"),
               menuSubItem("Por procedimento", tabName = "oferta_e_demanda_por_procedimento")),
            menuItem("Absenteísmo",tabName = "absenteismo", icon = icon("dashboard"),           
               menuSubItem("Por produção", tabName = "absenteismo_producao"),
               menuSubItem("Por solicitação", tabName = "absenteismo_solicitacao")), 
            #menuItem("Instruções", icon = icon("question-circle"),
                     #href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/wiki/Instru%C3%A7%C3%B5es-para-Utiliza%C3%A7%C3%A3o-das-Salas-de-Situa%C3%A7%C3%A3o-em-Sa%C3%BAde"),
            #menuItem("Dados", icon = icon("database"),
                     #href = "http://floripadadosabertos.univille.br/"),
            menuItem("Código-fonte", icon = icon("code"), 
                     href = "https://github.com/lpgarcia18/sala_situacao_regulacao"),
            menuItem("Licença de Uso", icon = icon("cc"), 
                     href = "https://github.com/lpgarcia18/lista_de_pacientes_ap/blob/master/LICENSE")
          )
        ),
        ########################################################################################### 
        dashboardBody(
          tabItems(
            ########################################################################################### 
            #Tempo de Espera por procedimento
            ###########################################################################################
            tabItem(tabName = "tempo_de_espera_procedimento_dia", h2("Tempo de Espera por Procedimento"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_tempo_de_espera_procedimento",
                        label="Selecione um Procedimento:",
                        choices= c(unique(as.character(previsao$PROCEDIMENTO)), "TODOS"),
                        selected="TODOS"),
                        width = 12)
                    ),
                    
                    fluidRow(
                      tabBox(title = "Oferta e Demanda de Procediemntos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "tempo_de_espera_procedimento_plot",height = "1000px")),
                             tabPanel("Dados", dataTableOutput("tempo_de_espera_procedimento_tab", height = "800px")),
                             tabPanel("Informações", htmlOutput("tempo_de_espera_procedimento_info", height = "800px"))
                             )
                           )
                        ),
            ########################################################################################### 
            #Oferta e Demanda por procedimento
            ###########################################################################################
            tabItem(tabName = "oferta_e_demanda_por_procedimento", h2("Oferta e Demanda por Procedimento"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_oferta_demanda_por_procedimento",
                        label="Selecione um Procedimento:",
                        choices= unique(oferta_e_demanda$PROCEDIMENTO),
                        selected=""),
                        width = 12)
                    ),
                    
                    fluidRow(
                      tabBox(title = "Oferta e Demanda de Procediemntos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "oferta_demanda_por_procedimento_plot",height = "600px")),
                             tabPanel("Dados", dataTableOutput("oferta_demanda_por_procedimento_tab", height = "600px")),
                             tabPanel("Informações", htmlOutput("oferta_demanda_por_procedimento_info", height = "600px"))
                             )
                           )
                        ),
            ########################################################################################### 
            #Absenteísmo por Procedimentos Produzidos
            ###########################################################################################
            tabItem(tabName = "absenteismo_producao", h2("Absenteísmo por Procedimentos Produzidos"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_procedimento_produzido",
                        label="Selecione uma Unidade:",
                        choices=list("POLICLÍNICA", "CEO", "EXTERNO", "POLICLÍNICA ou EXTERNO", "TODAS"),
                        selected="TODAS"),
                        width = 12)
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Absenteísmo por Procedimentos Produzidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "absenteismo_producao_plot",height = "1000px")),
                             tabPanel("Dados", dataTableOutput("absenteismo_producao_tab", height = "800px")),
                             tabPanel("Informações", htmlOutput("absenteismo_producao_info", height = "800px"))
                             )
                           )
                        ),
            ########################################################################################### 
            #Absenteísmo por Procedimentos Solicitado
            ###########################################################################################
            tabItem(tabName = "absenteismo_solicitacao", h2("Absenteísmo por Unidades Solicitantes"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_solicitacao_distrito",
                        label="Selecione um Distrito:",
                        choices=list("CENTRO", "CONTINENTE", "NORTE", "SUL", "TODOS"),
                        selected="TODOS"),
                        width = 12)
                    ),
                    
                     fluidRow(
                       box(selectInput(
                        inputId="lista_solicitacao_ano",
                        label="Selecione um Distrito:",
                        choices=list("ANO_2017", "ANO_2018", "TODOS"),
                        selected="TODOS"),
                        width = 12)
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Absenteísmo por Unidades Solicitantes", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "absenteismo_solicitacao_plot",height = "1000px")),
                             tabPanel("Dados", dataTableOutput("absenteismo_solicitacao_tab", height = "800px")),
                             tabPanel("Informações", htmlOutput("absenteismo_solicitacao_info", height = "800px"))
                             )
                           )
                        )

      )
   )
)
########################################################################################### 
server <- function(input, output, session) {
###########################################################################################
###########################################################################################
#Tempo de Espera por Procedimento
###########################################################################################

tempo_de_espera_procedimento <- reactive({
   req(input$lista_tempo_de_espera_procedimento)
   ifelse(input$lista_tempo_de_espera_procedimento == "TODOS", a <- previsao, 
      a <- subset(previsao, previsao$PROCEDIMENTO == input$lista_tempo_de_espera_procedimento))
      a$PROCEDIMENTO <- factor(a$PROCEDIMENTO, levels = a$PROCEDIMENTO[order(a$VALOR)])
      a
      
      
})
 

#gráfico 
output$tempo_de_espera_procedimento_plot <- renderPlotly({
   req(input$lista_tempo_de_espera_procedimento)
   if(input$lista_tempo_de_espera_procedimento == "TODOS"){
   
              a <-  ggplot(tempo_de_espera_procedimento(), aes(x =  PROCEDIMENTO, y = VALOR, fill = CLASSIFICACAO)) + 
                     geom_col()+ 
                     ylab("DIAS")+
                     xlab(" ")+
                     theme_classic()+
                     theme(axis.text.x = element_text(hjust = 1))+
                     coord_flip()+
                     scale_fill_manual(values = c("Adequado" = "green", "Alerta" = "yellow", "Inadequado" = "red"))

              ggplotly(a)
              
   }else{
   
              gg.gauge(tempo_de_espera_procedimento()$VALOR, as.character(tempo_de_espera_procedimento()$PROCEDIMENTO))
      
   }
    
   
 
})

#tabela 
output$tempo_de_espera_procedimento_tab <- renderDataTable({
 
 as.data.frame(tempo_de_espera_procedimento(), row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))



#informações 
output$tempo_de_espera_procedimento_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
   
###########################################################################################
#Oferta e Demanda por Procedimento
###########################################################################################
#Solução para construir o gráfico foi encontrada aqui: https://stackoverflow.com/questions/45227527/ggplot2-cannot-color-area-between-intersecting-lines-using-geom-ribbon?rq=1
banco <- reactive({
   req(input$lista_oferta_demanda_por_procedimento)
   oferta <- subset(oferta_e_demanda, oferta_e_demanda$TIPO == "Agendados" & oferta_e_demanda$PROCEDIMENTO == input$lista_oferta_demanda_por_procedimento)
   demanda <- subset(oferta_e_demanda, oferta_e_demanda$TIPO == "Solicitados" & oferta_e_demanda$PROCEDIMENTO == input$lista_oferta_demanda_por_procedimento)
   a <- merge(oferta, demanda, by = c("PROCEDIMENTO", "MES"))
   a <- a[,-c(3,5)]
   names(a)[c(3,4)]<- c("AGENDADO", "SOLICITADO")
   a
})
   
banco.interp <- reactive({
   map_df(banco(),~data.frame(AGENDADO = approx(banco()$MES, banco()$AGENDADO, n = 200), 
                     SOLICITADO = approx(banco()$MES, banco()$SOLICITADO, n = 200)))
})


banco.pre <- reactive({ 
   a <- banco.interp()[,-3]
   names(a) <- c("DATA", "AGENDADO", "SOLICITADO")
   a
})   

output$oferta_demanda_por_procedimento_plot <- renderPlotly({   
      a <- ggplot(banco.pre(), aes(banco.pre()$DATA))+
           geom_line(aes(y = banco.pre()$AGENDADO, group = 1, fill = "Agendado"), color = "blue", size = 1)+
           geom_line(aes(y = banco.pre()$SOLICITADO, group = 1, fill = "Solicitado"), color = "red", size = 1)+
           geom_ribbon(aes(ymin = banco.pre()$SOLICITADO, ymax = pmin(banco.pre()$AGENDADO, banco.pre()$SOLICITADO)), fill = "red", alpha = 0.3) +
           geom_ribbon(aes(ymin = banco.pre()$AGENDADO, ymax = pmin(banco.pre()$AGENDADO, banco.pre()$SOLICITADO)), fill = "blue", alpha = 0.3) +
           scale_fill_brewer(palette = "Set1", direction = -1)+
           theme_classic()+
           ylab("")+
           xlab("Data")
           expand_limits(y = 0)
           
      ggplotly(a, tooltip = NULL)
      
})
      
      
#tabela 
output$oferta_demanda_por_procedimento_tab <- renderDataTable({
 
 a <- banco()
 a$MES <- as.Date(a$MES, format = "%d/%m/%Y")
 a
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))



#informações 
output$oferta_demanda_por_procedimento_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
   
###########################################################################################
#Absenteísmo por Procedimentos Produzidos
###########################################################################################

absenteismo_procedimento <- reactive({
   req(input$lista_procedimento_produzido)
   ifelse(input$lista_procedimento_produzido == "TODAS", a <- absenteismo_analise_procedimento, 
      a <- subset(absenteismo_analise_procedimento, absenteismo_analise_procedimento$UNIDADE == input$lista_procedimento_produzido))
      a$PROCEDIMENTO <- factor(a$PROCEDIMENTO, levels = a$PROCEDIMENTO[order(a$`Percent Falta`)])
      a
      
      
})
 

#gráfico 
output$absenteismo_producao_plot <- renderPlotly({
   req(input$lista_procedimento_produzido)
   if(input$lista_procedimento_produzido == "TODAS"){
   
              a <-  ggplot(absenteismo_procedimento(), aes(x =  PROCEDIMENTO, y = `Percent Falta`, fill = UNIDADE)) + 
                     geom_col()+ 
                     ylab("  ")+
                     xlab("  ")+
                     theme_classic()+
                     theme(axis.text.x = element_text(hjust = 1))+
                     coord_flip()
   }else{
   
              a <-  ggplot(absenteismo_procedimento(), aes(x =  PROCEDIMENTO, y = `Percent Falta`, fill = `Percent Falta`)) + 
                  geom_col()+ 
                  ylab("  ")+
                  xlab("  ")+
                  theme_classic()+
                  theme(axis.text.x = element_text(hjust = 1))+
                  coord_flip()+ 
                  scale_fill_gradient(low = "green", high = "red")
   }
    
   ggplotly(a)
 
})

#tabela 
output$absenteismo_producao_tab <- renderDataTable({
 
 as.data.frame(absenteismo_procedimento(), row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))



#informações 
output$absenteismo_producao_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

###########################################################################################
#Absenteísmo por Procedimentos Solicitado
###########################################################################################

absenteismo_solicitacao_distrito <- reactive({
   req(input$lista_solicitacao_distrito)
   req(input$lista_solicitacao_ano)
   ifelse(input$lista_solicitacao_distrito == "TODOS", a <- absenteismo_analise_unidade_solicitante, 
      a <- subset(absenteismo_analise_unidade_solicitante, absenteismo_analise_unidade_solicitante$DISTRITO == input$lista_solicitacao_distrito))
      ifelse(input$lista_solicitacao_ano == "ANO_2018", 
             a$UNIDADE <- factor(a$UNIDADE, levels = a$UNIDADE[order(a$ANO_2018)]),
             a$UNIDADE <- factor(a$UNIDADE, levels = a$UNIDADE[order(a$ANO_2017)]))
      a
})

absenteismo_solicitacao <- reactive({
   req(input$lista_solicitacao_ano)
   ifelse(input$lista_solicitacao_ano == "TODOS", a <- absenteismo_solicitacao_distrito(), 
      a <- cbind(absenteismo_solicitacao_distrito()[,c(1,2)], absenteismo_solicitacao_distrito()[,names(absenteismo_solicitacao_distrito()) == input$lista_solicitacao_ano]))
      a
}) 


#gráfico 
output$absenteismo_solicitacao_plot <- renderPlotly({
   req(input$lista_solicitacao_distrito)
   req(input$lista_solicitacao_ano)
   if(input$lista_solicitacao_distrito == "TODOS" & input$lista_solicitacao_ano == "TODOS"){
      
               a <- ggparcoord(data = absenteismo_solicitacao(), columns = c(3,4), 
                     groupColumn = 1, order = 4, boxplot = T, showPoints = T, scale = "uniminmax")+
                     geom_text( aes(label = rep(absenteismo_solicitacao()$UNIDADE,2))) 
      
   }else if(input$lista_solicitacao_distrito != "TODOS" & input$lista_solicitacao_ano == "TODOS"){
   
               a <- ggparcoord(data = absenteismo_solicitacao(), columns = c(3,4), 
                     groupColumn = 2,order = 4, boxplot = T, showPoints = T, scale = "uniminmax")+
                     geom_text( aes(label = rep(absenteismo_solicitacao()$UNIDADE,2))) 
               
   }else if(input$lista_solicitacao_distrito == "TODOS" & input$lista_solicitacao_ano != "TODOS"){
      
              VALOR <- absenteismo_solicitacao()[,names(absenteismo_solicitacao())==input$lista_solicitacao_ano]
              
              a <-  ggplot(absenteismo_solicitacao(), aes(x =  UNIDADE, y = VALOR, fill = DISTRITO)) + 
                     geom_col()+ 
                     ylab("  ")+
                     xlab("  ")+
                     theme_classic()+
                     theme(axis.text.x = element_text(hjust = 1))+
                     coord_flip()
              
   }else{
      
              VALOR <- absenteismo_solicitacao()[,names(absenteismo_solicitacao())==input$lista_solicitacao_ano]   
   
              a <-  ggplot(absenteismo_solicitacao(), aes(x =  UNIDADE, y = VALOR, fill = VALOR)) + 
                  geom_col()+ 
                  ylab("  ")+
                  xlab("  ")+
                  theme_classic()+
                  theme(axis.text.x = element_text(hjust = 1))+
                  coord_flip()+ 
                  scale_fill_gradient(low = "green", high = "red")
   }
    
   ggplotly(a)
 
})

#tabela 
output$absenteismo_solicitacao_tab <- renderDataTable({
 
 absenteismo_solicitacao()
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))



#informações 
output$absenteismo_solicitacao_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

}    

###########################################################################################
shinyApp(ui, server)


