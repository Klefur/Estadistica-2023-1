library(shiny)
library(ggplot2)
library(moments)

# Rerefencias

# Modelo GPT-3.5, OpenAI (2021). ChatGPT. 
# [Software informático]. Disponible en https://openai.com

# Sharma, M. (30 de mayo de 2023). Cardiovascular Risk Data. 
# Recuperado de https://www.kaggle.com/datasets/mamta1999/cardiovascular-risk-data?resource=download 

directorio <- dirname(rstudioapi::getSourceEditorContext()$path)
archivo <- file.path(directorio, "cardiovascular.csv")
datos <- read.csv(archivo)

Mode <- function(x){
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

ui <- fluidPage(

    titlePanel("Datos de factor de riesgo cardiovascular"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Numero de datos:",
                        min = 1,
                        max = 500,
                        value = 250)
        ),

        mainPanel(
          selectInput("selectField","seleccionar variable",choices = setdiff(colnames(datos), "id")),
          plotOutput("distPlot"),
          verbatimTextOutput("cuantitativo"),
          dataTableOutput("tabla"),
          plotOutput("distPlot2")
        )
    )
)

server <- function(input, output) {
  
    output$cuantitativo <- renderText({
      columna <-datos[[input$selectField]]
      if (is.numeric(columna)){
        
        columna <- na.omit(columna)
        
        media <- mean(columna)
        mediana <- median(columna)
        moda <- Mode(columna)
        
        rango <- max(columna) - min(columna)
        desviacionEstandar <- sd(columna)
        varianza <- var(columna)
        cuartiles <- quantile(columna, prob = c(0.25, 0.5, 0.75))
        
        asimetria <- skewness(columna)
        curtosis <- kurtosis(columna)
        
        paste("La media es:", media, "\nLa mediana es:", mediana, 
              "\nLa moda es:", moda, "\nEl rango es:", rango,
              "\nLa desviacion estandar es:", desviacionEstandar,
              "\nLa varianza es: ", varianza,
              "\nLos cuartiles son: 25%:",cuartiles[1], "50%:", cuartiles[2], 
              "75%:", cuartiles[3], "\nLa asimetria es:", 
              asimetria, "\nLa curtosis es:", curtosis)
        
      } else {
        paste("La variable no es cuantitativa")
      }
    })
      
    output$tabla <- renderDataTable(datos)
    
    output$distPlot <- renderPlot({
      x    <- datos[,input$selectField]
      bins <- input$bins
      conteo <- table(datos[1:bins, input$selectField])
      
      resultados <- data.frame(dato = names(conteo),
                               frecuencia = as.numeric(conteo))
      
      # intente hacer que los datos numericos salieran ordenados
      # pero despues de probar varias cosas algunos no salen ordenados
      ggplot(data = resultados, aes(x = dato, y = frecuencia)) +
        geom_bar(stat = "identity", fill = "aquamarine") +
        labs(title = "Frecuencia de datos", x = "Dato", y = "Frecuencia")
      
      
    })
    output$distPlot2 <- renderPlot({
      x    <- faithful[, 2]
      plot(x)
    })
    
}

shinyApp(ui = ui, server = server)
