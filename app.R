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

########################################################################################### 
#Banco de dados 
###########################################################################################
source("dados_absenteismo.R")

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


