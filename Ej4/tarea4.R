library(shiny)
library(dplyr)

# Datos de ejemplo
datos <- data.frame(
  Nombre = c("Juan", "María", "Pedro", "Ana"),
  Edad = c(25, 30, 35, 40),
  Ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia")
)

ui <- fluidPage(
  selectInput("columna", "Selecciona una columna:", choices = colnames(datos)),
  verbatimTextOutput("resultado")
)

server <- function(input, output) {
  output$resultado <- renderPrint({
    columna_seleccionada <- input$columna
    datos_columna <- datos[[columna_seleccionada]]
    
    if (is.numeric(datos_columna)) {
      paste("La columna", columna_seleccionada, "contiene datos numéricos.")
    } else {
      paste("La columna", columna_seleccionada, "no contiene datos numéricos.")
    }
  })
}

shinyApp(ui, server)

