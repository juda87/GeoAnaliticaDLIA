#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
load(".RData_espumas")
tablaespumas <- tabla
load(".RData_curtiembres")

historicocalidad <- read_sf("../../PublicacionDatos/Compilado/salidas/historico_2014_2021_Parcial2022.gpkg")

historicocalidad$valor <- as.numeric(historicocalidad$valor)


ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      shinydashboard::menuItem("Series Históricas", tabName = "historicocal", icon = icon("dashboard")),
      shinydashboard::menuItem("Seguimiento a Curtiembres", tabName = "curtiembres", icon = icon("th")),
      shinydashboard::menuItem("Incidente 34", tabName = "espumas", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # # First tab content
      tabItem(tabName = "historicocal",
              fluidRow(
                "Historico Calidad Agua",
                sidebarPanel(
                  selectizeInput('punto', 'Seleccione puntos', choices = c("Seleccione" = "", c(unique(historicocalidad$punto))),multiple = TRUE),
                  selectizeInput('parametrohistorico', 'Seleccione parametros', choices = c("choose" = "", unique(historicocalidad$parametro_armonizado)), multiple = TRUE)
                  #              selectInput("punto", "Puntos:",
                  #                as.list(unique(Puntos$punto)), selectize = TRUE, 
                  #                multiple = TRUE,  width = 400),
                  # #   selectInput("punto", "Parametro:",
                  #               as.list(unique(historico$parametro_armonizado[historico$punto %in% input$punto])), selectize = TRUE, 
                  #               multiple = FALSE,  width = 400)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("plothistorico"),
                  tableOutput("tablahistorico")
                )
               
              )
      ),
      
      # Second tab content
      tabItem(tabName = "curtiembres",
              h2("Curtiembres"),
              selectInput(
                "tipoagua",
                "Tipo de agua",
                list("SUPERFICIAL","RESIDUAL"),
                selected = c("SUPERFICIAL","RESIDUAL"),
                multiple = TRUE,
                selectize = TRUE
              ),
              tmapOutput("mapacurtiembres")

      ),
      tabItem(tabName = "espumas",
              h2("Espumas"),
              selectInput(
                "parametroespumas",
                "Parámetro",choices = unique(tablaespumas$ID.de.la.Propiedad.observada),
                
                selectize = TRUE
              ),
              tmapOutput("mapaespumas"),
              plotOutput("barras")
      )
    )
  )
)
server <- shinyServer(function(input, output, session) {
  
  parametro.choice <- reactive({
    historicocalidad %>% 
      filter(punto %in% input$punto) %>%
      pull(parametro_armonizado) %>% unique()
  })
  
  serie <- reactive({ 
    tablahistorico <- subset(historicocalidad, punto %in% input$punto & parametro_armonizado %in% input$parametrohistorico)
    tablahistorico <- tablahistorico[!duplicated(tablahistorico$fechamuestreo),]
    zoo(tablahistorico$valor, tablahistorico$fechamuestreo)
    
  })
  
  observe({
    updateSelectizeInput(session, "parametrohistorico", choices = parametro.choice())
  }) 
  
  output$plothistorico <- renderPlot({
    print(autoplot(serie()) + ylab (input$parametrohistorico[1]) + xlab("Fecha") ) 
  })
  
  output$tablahistorico <- renderTable({ 
    
    datos <- fortify.zoo(serie())
    datos$Index <- as.character(datos$Index)
    datos
    
  }, rownames = TRUE)
  
  
  output$mapacurtiembres <- renderTmap({
    tm_basemap(server = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") +
      tm_shape( conductividadvp[conductividadvp$Tipo.de.agua %in% input$tipoagua,], name = "Conductividad") + tm_dots(size = "valornum",col = "yellow", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( dqovp[dqovp$Tipo.de.agua %in% input$tipoagua,] , name = "DQO") + tm_dots(size = "valornum",col = "red", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( dbovp[dbovp$Tipo.de.agua %in% input$tipoagua,], name = "DBO") + tm_dots(size = "valornum",col = "green", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( cromovp[cromovp$Tipo.de.agua %in% input$tipoagua,], name = "Cromo") + tm_dots(size = "valornum",col = "gray", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( phvp, name = "pH") + tm_dots(size = "valornum",col = "orange", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( sulfurovp[sulfurovp$Tipo.de.agua %in% input$tipoagua,], name = "Sulfuro") + tm_dots(size = "valornum",col = "beige", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( aygvp[aygvp$Tipo.de.agua %in% input$tipoagua,], name = "Aceites y Grasas") + tm_dots(size = "valornum",col = "peru", popup.vars = c("punto","municipio","parametro","unidades","valornum")) +
      tm_shape( odvp[odvp$Tipo.de.agua %in% input$tipoagua,], name = "Oxigeno Disuelto") + tm_dots(size = "valornum",col = "blue", popup.vars = c("punto","municipio","parametro","unidades","valornum")) 
  })
  
  output$mapaespumas <- renderTmap({
    caracteristicas <- c("Nombre.de.la.Ubicación.IDEAM", "Nombre.de.la.Ubicación.CAR", "Fecha.y.hora.de.la.Observación","Valor.del.resultado","Unidad.del.resultado",
                         "Laboratorio:.Condición.de.detección", "Laboratorio:.LCM" )
    filtro <- tablaespumas[tablaespumas$"ID.de.la.Propiedad.observada" == input$parametroespumas,]
    resultados <- filtro[!is.na(filtro$Valor.del.resultado),]
    nodetectados <- filtro[!is.na(filtro$`Laboratorio:.Condición.de.detección` == "NOT_DETECTED"),]
    tmap_mode("view")
    if(all(is.na(filtro$Valor.del.resultado))){
      mapa <- tm_basemap(server = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") +
        tm_shape(nodetectados, name = "No Detectado")+ tm_dots(size = 0.3,col = "white",  popup.vars = caracteristicas)
    }else{
      if(length(which(is.na(filtro$Valor.del.resultado))) > 0 & nrow(nodetectados) > 0){
        mapa <- tm_basemap(server = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") +
          tm_shape(nodetectados, name = "No Detectado")+ tm_dots(size = 0.3,col = "white", popup.vars = caracteristicas) +
          tm_shape(resultados, name = resultados$ID.de.la.Propiedad.observada[1]) + tm_dots(size = "Valor.del.resultado",col = "yellow", popup.vars = caracteristicas)}}
    if(nrow(nodetectados) == 0){
      mapa <- tm_basemap(server = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") +
        tm_shape(resultados, name = resultados$ID.de.la.Propiedad.observada[1]) + tm_dots(size = "Valor.del.resultado",col = "yellow", popup.vars = caracteristicas)}
    
    mapa
  })
  
  output$barras <- renderPlot({
    # generate bins based on input$bins from ui.R
    filtro <- tablaespumas[tablaespumas$"ID.de.la.Propiedad.observada" == input$parametroespumas,]
    Conteo <- length(which(!is.na(filtro$`Laboratorio:.Condición.de.detección`)))
    proporcion <- (Conteo/nrow(filtro))*100
    proporcion <- round(proporcion,2)
    deteccion <- data.frame(list("No Detectado" = proporcion, "Presencia" = abs(proporcion - 100))) %>% t() %>% as.data.frame()
    deteccion$Condicion <- row.names(deteccion)
    
    colnames(deteccion)[1] <- "Porcentaje"
    
    ggplot(data=deteccion, aes(x=Condicion, y=Porcentaje)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=Porcentaje), vjust=-0.3, size=3.55)
  })
  
})

shinyApp(ui = ui, server = server)

shinyApp(ui, server)