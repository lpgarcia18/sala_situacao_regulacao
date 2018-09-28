options(encoding = "UTF-8")

########################################################################################### 
#pacotes 
###########################################################################################
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(stringr)
library(DT)
library(plotly)
library(reshape2)


########################################################################################### 
#Banco de dados 
###########################################################################################
dados <- source("lista_devolvidos/dados_devolvidos.R")

###########################################################################################
#Login
###########################################################################################
#Baseado em https://stackoverflow.com/questions/43404058/starting-shiny-app-after-password-input-with-shinydashboard
Logged = FALSE
#my_username <- "teste"
#my_password <- "teste"
my_username <- senhas$nome
my_password <- senhas$senha

########################################################################################### 
#UI
###########################################################################################
########################################################################################### 
ui <- dashboardPage(skin = "blue",
########################################################################################### 
        dashboardHeader(title = "Pacientes Devolvidos - Regulação", titleWidth = 550),
        ########################################################################################### 
        dashboardSidebar(
          ########################################################################################### 
          sidebarMenu(
            menuItem("Florianóplis",tabName = "florianopolis", icon = icon("dashboard"),
            menuItem("Atenção primária",tabName = "atencao_primaria", icon = icon("dashboard"),
                     menuSubItem("Todos os distritos", tabName = "todos_distritos"),
                     menuSubItem("Por distrito", tabName = "por_distrito"),
                     menuSubItem("Todos centro de saúde", tabName = "centros_de_saude_distrito"),
                     menuSubItem("Por centro de saúde", tabName = "por_centros_de_saude")),
            menuItem("Atenção especializada",tabName = "atencao_especializada", icon = icon("dashboard"),
                     menuSubItem("Todas as policlínicas", tabName = "todas_policlinicas"))),
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
            #Todos os Distritos
            ###########################################################################################
            tabItem(tabName = "todos_distritos", h2("Pacientes Devolvidos - Todos os Distritos"),
                    
                    fluidRow(
                      tabBox(title = "Pacientes Devolvidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "todos_distritos_plot")),
                             tabPanel("Lista", dataTableOutput("todos_distritos_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("todos_distritos_info"))
                      )
                    )
            ),
            ########################################################################################### 
            #Por os Distritos
            ###########################################################################################
            tabItem(tabName = "por_distrito", h2("Pacientes Devolvidos - Por Distrito"),
                    
                    fluidRow(
                      tabBox(title = "Pacientes Devolvidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "por_distrito_plot")),
                             tabPanel("Lista", dataTableOutput("por_distrito_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("por_distrito_info"))
                      )
                    )
            ),
            ########################################################################################### 
            #Todos Centros de Saúde
            ###########################################################################################
            tabItem(tabName = "centros_de_saude_distrito", h2("Pacientes Devolvidos - Todos Centro de Saúde do Distrito"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_distrito",
                        label="Selecione um Distrito:",
                        choices=list("CENTRO" = "Centro\t", "CONTINENTE" = "Continente\t", "NORTE" = "Norte\t",
                                     "SUL" = "Sul\t"),
                        selected=""),
                        width = 12, status = "primary")
                    ),

                    fluidRow(
                      tabBox(title = "Pacientes Devolvidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "centros_de_saude_distrito_plot")),
                             tabPanel("Lista", dataTableOutput("centros_de_saude_distrito_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("centros_de_saude_distrito_info"))
                      )
                    )
            ),
            ########################################################################################### 
            #Por Centros de Saúde
            ###########################################################################################
            tabItem(tabName = "por_centros_de_saude", h2("Pacientes Devolvidos - Por Centro de Saúde do Distrito"),
                    
                  fluidRow(
                      box(selectInput(
                        inputId="lista_cs",
                        label="Selecione um Centro de Saúde:",
                        choices=list("CS ABRAAO" = "CS ABRAAO" ,  "CS AGRONOMICA"  = "CS AGRONOMICA",  "CS ALTO RIBEIRAO" = "CS ALTO RIBEIRAO",  
                                     "CS ARMACAO"  =  "CS ARMACAO",  "CS BALNEARIO" =  "CS BALNEARIO",  "CS BARRA DA LAGOA" = "CS BARRA DA LAGOA",
                                     "CS CACHOEIRA DO BOM JESUS" = "CS CACHOEIRA DO BOM JESUS", "CS CAEIRA DA BARRA DO SUL" = "CS CAEIRA DA BARRA DO SUL", 
                                     "CS CAMPECHE" =  "CS CAMPECHE",  "CS CANASVIEIRAS" = "CS CANASVIEIRAS",  "CS CANTO DA LAGOA" = "CS CANTO DA LAGOA",  
                                     "CS CAPOEIRAS"  =  "CS CAPOEIRAS",  "CS CARIANOS"  =  "CS CARIANOS",  "CS CENTRO" = "CS CENTRO",  
                                     "CS COLONINHA" = "CS COLONINHA",  "CS COQUEIROS"  = "CS COQUEIROS",  "CS CORREGO GRANDE" = "CS CORREGO GRANDE",  
                                     "CS COSTA DA LAGOA" =  "CS COSTA DA LAGOA",  "CS COSTEIRA DO PIRAJUBAE"="CS COSTEIRA DO PIRAJUBAE",  
                                     "CS ESTREITO" = "CS ESTREITO",  "CS FAZENDA DO RIO TAVARES" = "CS FAZENDA DO RIO TAVARES", "CS INGLESES"= "CS INGLESES",  
                                     "CS ITACORUBI"= "CS ITACORUBI",  "CS JARDIM ATLANTICO"= "CS JARDIM ATLANTICO",  "CS JOAO PAULO"= "CS JOAO PAULO",  "CS JURERE" ="CS JURERE" ,
                                     "CS LAGOA DA CONCEICAO"= "CS LAGOA DA CONCEICAO", "CS MONTE CRISTO"= "CS MONTE CRISTO",  "CS MONTE SERRAT"= "CS MONTE SERRAT",  
                                     "CS MORRO DAS PEDRAS"= "CS MORRO DAS PEDRAS",  "CS NOVO CONTINENTE"= "CS NOVO CONTINENTE",  "CS PANTANAL" =  "CS PANTANAL",  
                                     "CS PANTANO DO SUL" = "CS PANTANO DO SUL", "CS PONTA DAS CANAS"=  "CS PONTA DAS CANAS",  "CS PRAINHA"=  "CS PRAINHA",  "CS RATONES"=  "CS RATONES",  
                                     "CS RIBEIRAO DA ILHA"= "CS RIBEIRAO DA ILHA",  "CS RIO TAVARES"=  "CS RIO TAVARES",  "CS RIO VERMELHO"= "CS RIO VERMELHO",  
                                     "CS SACO DOS LIMOES"= "CS SACO DOS LIMOES",  "CS SACO GRANDE"= "CS SACO GRANDE",  "CS SANTINHO"=  "CS SANTINHO",  
                                     "CS SANTO ANTONIO DE LISBOA" = "CS SANTO ANTONIO DE LISBOA", "CS SAPE"  = "CS SAPE" , "CS TAPERA"=  "CS TAPERA",  "CS TRINDADE"= "CS TRINDADE",  
                                     "CS VARGEM GRANDE"= "CS VARGEM GRANDE", "CS VARGEM PEQUENA"=  "CS VARGEM PEQUENA",  "CS VILA APARECIDA" = "CS VILA APARECIDA"),
                        selected="CS ABRAÃO"),
                        width = 12, status = "primary")
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Pacientes Devolvidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "por_centros_de_saude_plot")),
                             tabPanel("Lista", dataTableOutput("por_centros_de_saude_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("por_centros_de_saude_info"))
                      )
                    )
            ),
            ###########################################################################################
            #Todas as Policlínicas
            ###########################################################################################
            tabItem(tabName = "todas_policlinicas", h2("Pacientes Devolvidos - Todas as Policlínicas"),
                    
                    fluidRow(
                      tabBox(title = "Pacientes Devolvidos", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "todas_policlinicas_plot")),
                             tabPanel("Lista - Policlínica Centro", dataTableOutput("policlinica_centro_tab")),
                             tabPanel("Lista - Policlínica Continente", dataTableOutput("policlinica_continente_tab")),
                             tabPanel("Lista - Policlínica Norte", dataTableOutput("policlinica_norte_tab")),
                             tabPanel("Lista - Policlínica Sul", dataTableOutput("policlinica_sul_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("todas_policlinicas_info"))
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
#Login
###########################################################################################
values <- reactiveValues(authenticated = FALSE)

# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("username", "Nome:"),
    passwordInput("password", "Senha:"),
    footer = tagList(
      #modalButton("Cancel"),
      actionButton("entrar", "Entrar")
    )
  )
}

# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential

obs1 <- observe({
  showModal(dataModal())
})

# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 

obs2 <- observe({
  req(input$entrar)
  isolate({
    Username <- input$username
    Password <- input$password
  })
  Id.username <- if(Username %in% my_username){which(my_username == Username)}
  Id.password <- if(Password %in% my_password){which(my_password == Password)}
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()

    } else {
      values$authenticated <- FALSE
    }     
  }
})

###########################################################################################
#Todos os Distritos
###########################################################################################

#gráfico 
output$todos_distritos_plot <- renderPlotly({
 
 todos_distritos_lista <- serie_historica_todos_distritos 
 todos_distritos_lista <- melt(todos_distritos_lista) 
 names(todos_distritos_lista) <- c("DATA", "VALOR")
 
 
 a <- ggplot(todos_distritos_lista, aes(x = DATA, y = VALOR, group = 1)) + 
         geom_line(colour = "blue")+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         expand_limits(y = 0)
       
 ggplotly(a)
 
})

#tabela 
output$todos_distritos_tab <- renderDataTable({
 
 devolvidos <- subset(devolvidos, !is.na(devolvidos$`DISTRITO	`))
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$todos_distritos_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
###########################################################################################
#Todos os Distritos
###########################################################################################

#gráfico 
output$por_distrito_plot <- renderPlotly({
 
 por_distrito_lista <- serie_historica_distritos 
 por_distrito_lista <- melt(por_distrito_lista) 
 names(por_distrito_lista) <- c("DISTRITO", "DATA", "VALOR")
 por_distrito_lista$DISTRITO <- as.factor(por_distrito_lista$DISTRITO) 
 
 
 a <- ggplot(por_distrito_lista, aes(x = DATA, y = VALOR, group = DISTRITO)) + 
         geom_line(aes(colour = por_distrito_lista$DISTRITO))+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         expand_limits(y = 0)
       
 ggplotly(a)
 
})

#tabela 
output$por_distrito_tab <- renderDataTable({
 
  devolvidos <- subset(devolvidos, !is.na(devolvidos$`DISTRITO	`))
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$por_distrito_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
###########################################################################################       
#Todos Centros de Saúde
########################################################################################### 
#gráfico 
output$centros_de_saude_distrito_plot <- renderPlotly({
 
 centros_de_saude_lista <- serie_historica
 names(centros_de_saude_lista)[7] <- "DISTRITO"
 centros_de_saude_lista <- filter(centros_de_saude_lista, centros_de_saude_lista$DISTRITO == input$lista_distrito)
 centros_de_saude_lista <- centros_de_saude_lista[,-7]
 centros_de_saude_lista <- melt(centros_de_saude_lista) 
 names(centros_de_saude_lista) <- c("UNIDADE", "DATA", "VALOR")
 centros_de_saude_lista <- aggregate(centros_de_saude_lista$VALOR, by = list(centros_de_saude_lista$UNIDADE, centros_de_saude_lista$DATA), FUN = sum, na.rm=TRUE, na.action=NULL)
 names(centros_de_saude_lista) <- c("UNIDADE", "DATA", "VALOR")

 
 a <- ggplot(centros_de_saude_lista, aes(x = DATA, y = VALOR, group = UNIDADE)) + 
         geom_line(aes(colour = centros_de_saude_lista$UNIDADE))+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()
       
 ggplotly(a)

 
})

#tabela 
output$centros_de_saude_distrito_tab <- renderDataTable({
 
 devolvidos <- filter(devolvidos, devolvidos$DISTRITO == input$lista_distrito)
 as.data.frame(devolvidos)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$centros_de_saude_distrito_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
###########################################################################################
#Por centro de saúde
###########################################################################################
#gráfico 
output$por_centros_de_saude_plot <- renderPlotly({
 
 centros_de_saude_lista <- serie_historica
 centros_de_saude_lista <- filter(centros_de_saude_lista, centros_de_saude_lista$UNIDADE == input$lista_cs)
 centros_de_saude_lista <- centros_de_saude_lista[,-7]
 centros_de_saude_lista <- melt(centros_de_saude_lista) 
 names(centros_de_saude_lista) <- c("UNIDADE", "DATA", "VALOR")
 
 
 a <- ggplot(centros_de_saude_lista, aes(x = DATA, y = VALOR, group = UNIDADE)) + 
         geom_line(aes(colour = centros_de_saude_lista$UNIDADE))+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()
       
 ggplotly(a)

 
})

#tabela 
output$por_centros_de_saude_tab <- renderDataTable({
 
 devolvidos <- filter(devolvidos, devolvidos$UNIDADE == input$lista_cs)
 as.data.frame(devolvidos)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$por_centros_de_saude_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

###########################################################################################       
#Todas policlinicas
########################################################################################### 
#gráfico 
output$todas_policlinicas_plot <- renderPlotly({
 
 policlinicas_lista <- devolvidos
 policlinicas_lista <- subset(policlinicas_lista, grepl("POLICLINICA", UNIDADE))
 policlinicas_lista$VALOR <- 1
 policlinicas_lista <- aggregate(policlinicas_lista$VALOR, by = list(policlinicas_lista$UNIDADE), FUN = sum, na.rm=TRUE)
 names(policlinicas_lista) <- c("UNIDADE", "VALOR")
 policlinicas_lista$UNIDADE <- factor(policlinicas_lista$UNIDADE, levels = policlinicas_lista$UNIDADE[order(policlinicas_lista$VALOR,decreasing = T)])
 
  a <- ggplot(policlinicas_lista, aes(x = UNIDADE, y = VALOR, fill = UNIDADE)) + 
         geom_col()+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()
       
 ggplotly(a)

 
})

#tabela 
output$policlinica_centro_tab <- renderDataTable({
 
 policlinicas_lista <- devolvidos[,-c(8,9)]
 policlinicas_lista <- subset(policlinicas_lista, policlinicas_lista$UNIDADE == "POLICLINICA MUNICIPAL CENTRO")
 as.data.frame(policlinicas_lista)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))

#tabela 
output$policlinica_continente_tab <- renderDataTable({
 
 policlinicas_lista <- devolvidos[,-c(8,9)]
 policlinicas_lista <- subset(policlinicas_lista, policlinicas_lista$UNIDADE == "POLICLINICA MUNICIPAL CONTINENTE")
 as.data.frame(policlinicas_lista)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))

#tabela 
output$policlinica_norte_tab <- renderDataTable({
 
 policlinicas_lista <- devolvidos[,-c(8,9)]
 policlinicas_lista <- subset(policlinicas_lista, policlinicas_lista$UNIDADE == "POLICLINICA MUNICIPAL NORTE")
 as.data.frame(policlinicas_lista)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))

#tabela 
output$policlinica_sul_tab <- renderDataTable({
 
 policlinicas_lista <- devolvidos[,-c(8,9)]
 policlinicas_lista <- subset(policlinicas_lista, policlinicas_lista$UNIDADE == "POLICLINICA MUNICIPAL SUL")
 as.data.frame(policlinicas_lista)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$todas_policlinicas_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

}    

###########################################################################################
shinyApp(ui, server)


