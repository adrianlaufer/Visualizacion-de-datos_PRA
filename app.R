library(shinythemes)
library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(viridis)
library(rmapshaper)  
library(cowplot)     

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
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Aplicar un tema predefinido para el estilo
  
  # Agregamos CSS
  tags$style(HTML("
    .container-custom {
      display: flex;
      flex-direction: column;
      gap: 15px; /* Espacio entre los paneles */
    }
    .main-panel-custom {
      background-color: #f8f9fa;
      border: 1px solid #e3e3e3;
      border-radius: 10px;
      padding: 15px;
      margin-top: 15px;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
      flex: 1; /* Ocupa todo el espacio disponible */
    }
    .sidebar-panel-custom {
      background-color: #f8f9fa;
      border: 1px solid #e3e3e3;
      border-radius: 10px;
      padding: 15px;
      margin-top: 15px;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
      flex: 0 0 300px; /* Ancho fijo del sidebar */
      display: flex;
      flex-direction: column;
    }
    .equal-height {
      display: flex;
      height: 100%;
      gap: 15px;
    }
  ")),
  
  titlePanel("Análisis de la distribución de residuos urbanos por comunidad autónoma"),
  
  fluidRow(
    column(4,
           div(class = "sidebar-panel-custom", 
               selectInput("residuo",
                           "Selecciona Residuo:",
                           choices = sort(unique(datos$Residuo))),
               fluidRow(
                 column(6,
                        radioButtons("metrica", "Selecciona Métrica:",
                                     choices = c("Total de residuos en [to]" = "Total",
                                                 "Toneladas por habitante" = "Porcentaje_por_poblacion"))
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
           )
    ),
    
    column(8,
           div(class = "main-panel-custom",
               fluidRow(
                 column(6,
                        plotOutput(outputId = "map", height = "846px")
                 ),
                 
                 column(6,
                        plotlyOutput(outputId = "bubble_plot", height = "846px")
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
  
  # Creamos el mapa con ggplot2
  output$map <- renderPlot({
    data_for_map <- filtered_data_for_map()
    datos_max <- filtered_data_for_table()
    
    if (input$metrica == "Porcentaje_por_poblacion") {
      data_for_map$metric <- data_for_map$Porcentaje_por_poblacion
    } else {
      data_for_map$metric <- data_for_map$Total
    }
    
    if (input$metrica == "Porcentaje_por_poblacion") {
      max_total <- max(datos_max$Porcentaje_por_poblacion, na.rm = TRUE)
    } else {
      max_total <- max(datos_max$Total, na.rm = TRUE)
    }
    
    comunidades_merged <- merge(comunidades, data_for_map, by.x = "NAMEUNIT", by.y = "Comunidad", all.x = TRUE)
    canarias_merged <- merge(canarias, data_for_map, by.x = "NAMEUNIT", by.y = "Comunidad", all.x = TRUE)
    
    plot_penin <- ggplot() +
      geom_sf(data = comunidades_merged, aes(fill = metric), color = "#000000", size = 0.2) +
      scale_fill_viridis_c(option = "YlOrRd", 
                           na.value = "#FFFFFF",
                           limits = c(0, max_total), 
                           name = if (input$metrica == "Porcentaje_por_poblacion") "Toneladas por habitante" else "Total de residuos en [to]",
                           guide = guide_colorbar(barwidth = 15, barheight = 1)) + 
      theme_minimal() +
      theme(
        panel.grid = element_blank(),             
        axis.title = element_blank(),            
        axis.text = element_blank(),              
        axis.ticks = element_blank(),             
        legend.position = "top",
        legend.text = element_text(size = 12),    
        legend.title = element_text(size = 14),   
        legend.key.width = unit(2, "cm"),         
        plot.title = element_text(size = 14)      
      ) +
      ggtitle(paste(if(input$metrica =="Porcentaje_por_poblacion"){"Toneladas por habitante en\n"} else {"Toneladas totales de\n"},input$residuo, "para el año", input$year))
    
    plot_canarias <- ggplot() +
      geom_sf(data = canarias_merged, aes(fill = metric), color = "#000000", size = 0.2) +
      scale_fill_viridis_c(option = "YlOrRd", na.value = "#FFFFFF", limits = c(0, max_total), guide = "none") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),             
        axis.title = element_blank(),             
        axis.text = element_blank(),              
        axis.ticks = element_blank(),             
        plot.title = element_text(size = 14)      
      ) 
    
    plot_grid(
      plot_penin + theme(), 
      plot_canarias + theme(),
      ncol = 1, 
      rel_heights = c(4, 1, 0.5)
    )
  })
  
  # Renderizamos el gráfico de líneas
  output$plot <- renderPlotly({
    data_for_plot <- filtered_data_for_table()
    
    if (input$metrica == "Porcentaje_por_poblacion") {
      data_for_plot$metric <- data_for_plot$Porcentaje_por_poblacion
      y_title <- "Toneladas por habitante"
    } else {
      data_for_plot$metric <- data_for_plot$Total
      y_title <- "Toneladas de residuos"
    }
    
    maximo <- max(data_for_plot$metric, na.rm = TRUE) 
    
    if (input$comunidad == "Todas las comunidades") {
      data_for_plot <- data_for_plot %>% filter(Residuo == input$residuo)
    } else {
      data_for_plot <- data_for_plot %>% filter(Comunidad == input$comunidad, Residuo == input$residuo)
    }
    
    plot_ly(data_for_plot, x = ~Periodo, y = ~metric, color = ~Comunidad,
            text = ~paste("Comunidad: ", Comunidad, "<br>", y_title, ": ", metric),
            type = "scatter", mode = "lines+markers") %>%
      layout(
        title = list(
          text = paste("Evolución de", input$residuo, "en<br>", input$comunidad),
          font = list(size = 14)),
        xaxis = list(title = "Año"),
        yaxis = list(title = y_title, range = c(0, maximo)),
        legend = list(title = list(text = "Comunidad"), 
                      x = 0.5, 
                      y = -0.2, 
                      xanchor = 'center', 
                      traceorder = "normal", 
                      orientation = 'h'),
        margin = list(l = 50, r = 50, b = 50, t = 100),
        autosize = TRUE
      ) %>%
      colorbar(title = list(text = y_title), x = 1.2, y = 0.5)
  })
  
  # Renderizamos el gráfico de burbujas
  output$bubble_plot <- renderPlotly({
    data_for_bubble_plot <- filtered_data_for_map()
    datos_max <- filtered_data_for_table()
    
    if (input$metrica == "Porcentaje_por_poblacion") {
      max_total <- max(datos_max$Porcentaje_por_poblacion, na.rm = TRUE)
    } else {
      max_total <- max(datos_max$Total, na.rm = TRUE)
    }
    
    # Determinar el color de las burbujas basado en la métrica seleccionada
    color_metric <- if (input$metrica == "Porcentaje_por_poblacion") {
      data_for_bubble_plot$Porcentaje_por_poblacion
    } else {
      data_for_bubble_plot$Total
    }
    
    plot <- plot_ly(
      data_for_bubble_plot, 
      x = ~PIB_PC, 
      y = ~Porcentaje_por_poblacion, 
      size = ~Poblacion,
      color = ~color_metric,
      colors = viridis(256, option = "YlOrRd"),  
      text = ~paste(
        "Comunidad: ", Comunidad, "<br>",
        "PIB per cápita [€]: ", PIB_PC, "<br>",
        "Toneladas por habitante: ", Porcentaje_por_poblacion, "<br>",
        "Población: ", Poblacion, "<br>",
        if (input$metrica == "Porcentaje_por_poblacion") {
          paste("Toneladas por habitante: ", Porcentaje_por_poblacion)
        } else {
          paste("Total de residuos [to]: ", Total)
        }
      ),
      type = 'scatter', 
      mode = 'markers', 
      marker = list(
        sizemode = 'diameter', 
        opacity = 0.7,
        colorbar = list(title = NULL, len = 15),  
        showscale = FALSE 
      )
    ) %>%
      layout(
        title = list(
          text = paste("Relación entre PIB per cápita y la toneladas por habitante en<br>", input$residuo, " para el año ", input$year),
          font = list(size = 14)
        ),
        xaxis = list(title = "PIB per cápita [€]"),
        yaxis = list(title = "Tonelada por habitante"),
        margin = list(l = 50, r = 50, b = 100, t = 100)
      )
    plot
  })
}

# Ejecutamos la aplicación
shinyApp(ui = ui, server = server)



