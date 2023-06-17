#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(moments)
library(e1071)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Datos de factor de riesgo cardiovascular"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 500,
                        value = 250)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          selectInput("selectField","seleccionar variable",choices = setdiff(colnames(datos), "id")),
          plotOutput("distPlot"),
          textOutput("cuantitativo"),
          dataTableOutput("tabla"),
          plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    directorio <- dirname(rstudioapi::getSourceEditorContext()$path)
    archivo <- file.path(directorio, "cardiovascular.csv")
    datos <- read.csv(archivo)
    
    Mode <- function(x){
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    output$cuantitativo <- renderText({
      columna <-datos[[input$selectField]]
      if (is.numeric(columna)){
        media <- mean(columna)
        mediana <- median(columna)
        moda <- Mode(columna)
        
        rango <- max(columna) - min(columna)
        desviacionEstandar <- sd(columna)
        varianza <- var(columna)
        cuartiles <- quantile(columna, prob = c(0.25, 0.5, 0.75))
        
        asimetria <- skewness(columna)
        curtosis <- kurtosis(columna)
        
        texto <- c(
          "La media es: ", "La mediana es: ", "La moda es: ", "El rango es: ",
          "La desviacion estandar es: ", "La varianza es: ", 
          "Los cuartiles son: ", "La asimetria es: ", "La curtosis es: "
          )
        
        variables <- c(media, mediana, moda, rango, desviacionEstandar, varianza,
                       cuartiles, asimetria, curtosis)
        
      texto_variables <- paste(texto, variables, sep=" ")
      texto_variables <- paste(texto_variables, collapse="\n")
      print(texto_variables)
      texto_variables
        
      } else {
        paste("La variable no es cuantitativa")
      }
    })
      
    output$tabla <- renderDataTable(datos)
    
    output$distPlot <- renderPlot({
      x    <- datos[,input$selectField]
      bins <- input$bins
      conteo <- table(datos[1:bins, input$selectField])
      
      # Crear un nuevo dataframe con los resultados
      resultados <- data.frame(dato = names(conteo),
                               frecuencia = as.numeric(conteo))
      
      # Graficar los resultados
      ggplot(data = resultados, aes(x = dato, y = frecuencia)) +
        geom_bar(stat = "identity", fill = "aquamarine") +
        labs(title = "Frecuencia de datos", x = "Dato", y = "Frecuencia")
      
      
    })
    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      plot(x)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
