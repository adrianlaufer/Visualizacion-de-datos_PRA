library(shinythemes)
library(shiny)
library(dplyr)
library(shinyjs)
library(DT)
library(sf)
library(ggplot2)
library(rmapshaper)  # Para simplificar polígonos
library(cowplot)     # Para combinar los plots
library(plotly)

# Cargamos los datos
datos <- read.csv('datos.csv', sep = ",")

# Cargamos y simplificamos los distritos
comunidades <- st_read("recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>% 
  st_transform(crs = 4326) %>%
  ms_simplify(keep = 0.05)  # Simplifica los polígonos al 5%

canarias <- st_read("recintos_autonomicas_inspire_canarias_regcan95/recintos_autonomicas_inspire_canarias_regcan95.shp") %>%
  st_transform(crs = 4326) %>%
  ms_simplify(keep = 0.05)  # Simplifica los polígonos al 5%

# Definimos la User Interface

# Definimos la User Interface
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Apply a predefined theme for styling
  
  # Add custom CSS for the main panel
  tags$style(HTML("
    .main-panel-custom {
      background-color: #f8f9fa;
      border: 1px solid #d3d3d3;
      border-radius: 5px;
      padding: 15px;
      margin-top: 15px;
    }
  ")),
  
  titlePanel("Análisis de la distribución de residuos urbanos"),
  
  sidebarLayout(
    sidebarPanel(
               selectInput("residuo",
                           "Selecciona Residuo:",
                           choices = sort(unique(datos$Residuo))),
      fluidRow(
        column(6,
               radioButtons("metrica", "Selecciona Métrica:",
                            choices = c("Total de residuos" = "Total",
                                        "Porcentaje de residuo por habitante" = "Porcentaje_por_poblacion"))
        ),
        column(6,
               sliderInput("year",
                           "Selecciona Año:",
                           min = min(datos$Periodo),
                           max = max(datos$Periodo),
                           value = min(datos$Periodo),
                           step = 1)
        )
      ),
      selectInput("comunidad",
                  "Selecciona Comunidad:",
                  choices = c("Todas las comunidades", unique(datos$Comunidad))),
      plotlyOutput(outputId = "plot", height = "600px")
    ),
    
    mainPanel(
      div(class = "main-panel-custom",
          fluidRow(
            column(6,
                   plotOutput(outputId = "map", height = "800px")
            ),
            
            column(6,
                   plotlyOutput(outputId = "bubble_plot", height = "800px")
            )
          )
      )
    )
  )
)

# Definimos el servidor lógico
server <- function(input, output, session) {
  
  # Filtramos los datos
  filtered_data_for_table <- reactive({
    req(input$comunidad, input$year, input$residuo)
    datos %>% filter(Residuo == input$residuo)
  })
  
  filtered_data_for_map <- reactive({
    req(input$year, input$residuo)
    datos %>% filter(Periodo == input$year,
                     Residuo == input$residuo)
  })
  
  max_total <- reactive({
    datos_max <- datos %>% filter(Residuo == input$residuo)
    max(datos_max$Total, na.rm = TRUE)
  })
  
  # Creamos el mapa con ggplot2
  output$map <- renderPlot({
    data_for_map <- filtered_data_for_map()
    
    if (input$metrica == "Porcentaje_por_poblacion") {
      data_for_map$metric <- data_for_map$Porcentaje_por_poblacion
    } else {
      data_for_map$metric <- data_for_map$Total
    }
    
    comunidades_merged <- merge(comunidades, data_for_map, by.x = "NAMEUNIT", by.y = "Comunidad", all.x = TRUE)
    canarias_merged <- merge(canarias, data_for_map, by.x = "NAMEUNIT", by.y = "Comunidad", all.x = TRUE)
    
    plot_penin <- ggplot() +
      geom_sf(data = comunidades_merged, aes(fill = metric), color = "#000000", size = 0.2) +
      scale_fill_viridis_c(option = "YlOrRd", na.value = "#FFFFFF", name = if (input$metrica == "Porcentaje_por_poblacion") "Porcentaje" else "Total de residuos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 12),  # Tamaño de la fuente de la leyenda
            legend.title = element_text(size = 14), # Tamaño del título de la leyenda
            legend.key.width = unit(2, "cm"),      # Ancho de la clave de la leyenda
            plot.title = element_text(size = 12)) +
      ggtitle(paste("Porcentaje por habitante de residuos ", input$residuo))
    
    plot_canarias <- ggplot() +
      geom_sf(data = canarias_merged, aes(fill = metric), color = "#000000", size = 0.2) +
      scale_fill_viridis_c(option = "YlOrRd", na.value = "#FFFFFF", guide = "none") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 12))  # Tamaño del título del gráfico
    
    # Ajustar el tamaño de la leyenda
    legend_penin <- get_legend(plot_penin)
    
    # Combinar los dos plots usando cowplot
    plot_grid(plot_penin + theme(legend.position = "none"), 
              plot_canarias + theme(legend.position = "bottom"),
              legend_penin, 
              ncol = 1, 
              rel_heights = c(3, 1, 0.5))
  })
  
  
  output$plot <- renderPlotly({
    data_for_plot <- filtered_data_for_table()
    
    if (input$metrica == "Porcentaje_por_poblacion") {
        data_for_plot$metric <- data_for_plot$Porcentaje_por_poblacion
        y_title <- "Porcentaje de residuo por población"
    } else {
        data_for_plot$metric <- data_for_plot$Total
        y_title <- "Toneladas de residuos"
    }
    maximo = max(data_for_plot$metric, na.rm = TRUE) # Ensure NA values are handled
    
    if(input$comunidad == "Todas las comunidades"){
        data_for_plot <- data_for_plot %>% filter(Residuo == input$residuo)
    } else {
        data_for_plot <- data_for_plot %>% filter(Comunidad == input$comunidad,
                                                  Residuo == input$residuo)
    }
    
    plot_ly(data_for_plot, x = ~Periodo, y = ~metric, color = ~Comunidad,
            text = ~paste("Comunidad: ", Comunidad, "<br>", y_title, ": ", metric),
            type = "scatter", mode = "lines+markers") %>%
      layout(title = paste("Evolución de", input$residuo, "en<br>", input$comunidad),
             xaxis = list(title = "Año"),
             yaxis = list(title = y_title, range = c(0, maximo)),
             legend = list(title = list(text = "Comunidad"), 
                           x = 0.5, 
                           y = -0.2, 
                           xanchor = 'center', 
                           traceorder = "normal", 
                           orientation = 'h'),
             margin = list(l = 50, r = 50, b = 50, t = 100),
             autosize = TRUE) %>%  # Ensures the plot resizes appropriately
      colorbar(title = list(text = y_title), x = 1.2, y = 0.5)
})

  
  

  output$bubble_plot <- renderPlotly({
    data_for_bubble_plot <- filtered_data_for_map()
    
    plot <- plot_ly(data_for_bubble_plot, x = ~PIB_PC, y = ~Porcentaje_por_poblacion, size = ~Poblacion,
                    text = ~paste("Comunidad: ", Comunidad, "<br>PIB per cápita: ", PIB_PC, "<br>Porcentaje por población: ", Porcentaje_por_poblacion, "<br>Población: ", Poblacion),
                    type = 'scatter', mode = 'markers', marker = list(sizemode = 'diameter', opacity = 0.7)) %>%
      layout(title = list(
        text = paste("Relación entre PIB per cápita y el porcentaje de residuos por habitante en<br>", input$residuo, " para el año ", input$year),
        font = list(size = 14)  # Tamaño del título
      ),
      xaxis = list(title = "PIB per cápita"),
      yaxis = list(title = "Porcentaje por población"),
      margin = list(l = 50, r = 50, b = 100, t = 100))
    
    plot
  })
}
# Ejecutamos la aplicación
shinyApp(ui = ui, server = server)

