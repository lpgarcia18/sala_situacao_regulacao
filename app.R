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
            #Centros de Saúde
            ###########################################################################################
            tabItem(tabName = "absenteismo_producao", h2("Absenteísmo por Procedimentos Produzidos"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_procedimento",
                        label="Selecione uma Unidade:",
                        choices=list("POLICLÍNICA", "CEO", "EXTERNO", "POLICLÍNICA ou EXTERNO", "TODAS"),
                        selected="TODAS"),
                        width = 12)
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Absenteísmo por Procedimentos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "absenteismo_producao_plot",height = "1000px")),
                             tabPanel("Dados", dataTableOutput("absenteismo_producao_tab", height = "800px")),
                             tabPanel("Informações", htmlOutput("absenteismo_producao_info", height = "800px"))
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
#Florianópolis
###########################################################################################

absenteismo_procedimento <- reactive({
   ifelse(input$lista_procedimento == "TODAS", a <- absenteismo_analise_procedimento, 
      a <- subset(absenteismo_analise_procedimento, absenteismo_analise_procedimento$UNIDADE == input$lista_procedimento))
      a
})
 

#gráfico 
output$absenteismo_producao_plot <- renderPlotly({
   
   a <- ggplot(absenteismo_procedimento(), aes(x =  PROCEDIMENTO, y = `Percent Falta`, fill = UNIDADE)) + 
         geom_col()+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         theme(axis.text.x = element_text(hjust = 1))+
         coord_flip()
       
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


}    

###########################################################################################
shinyApp(ui, server)


