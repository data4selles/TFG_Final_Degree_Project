library("shiny")
library("SampleSizeMeans")
library(ggplot2)
library(shinythemes)

# Definición del UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Aplicar un tema
  titlePanel("Cálculo del Tamaño Muestral para estimación de una media"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("v", "v:", 2, min = 0),
      numericInput("rho", "rho:", 2),
      numericInput("long", "longitud del intervalo:", 0.2),
      numericInput("n0", "n0:", 10),
      sliderInput("alpha", "Alpha:", min = 0, max = 0.2, value = 0.05, step = 0.01),
      actionButton("calculate", "Calcular")
    ),
    mainPanel(
      fluidRow(
        column(12, uiOutput("table_with_title"))
      ),
      fluidRow(
        column(12, plotOutput("sample_size_plot"))
      )
    )
  )
)

# Definición del servidor
server <- function(input, output) {
  observeEvent(input$calculate, {
    v <- input$v
    rho <- input$rho
    len <- input$long
    n0 <- input$n0
    alpha <- input$alpha
    lambda <- v/rho
    
    results <- data.frame(
      Criterio = c("Frecuentista", "Bayes (sigma conocido)", "ACC", "ALC", "WOC(90%)", "WOC(95%)"),
      n = c(mu.freq(len, lambda, 1- alpha),
            mu.freq(len, lambda, 1- alpha) - n0,
            mu.acc(len, v, rho, n0, 1- alpha), 
            mu.alc(len, v, rho, n0, 1- alpha),  
            mu.modwoc(len, v, rho, n0, 1- alpha, 0.9),
            mu.modwoc(len, v, rho, n0, 1- alpha, 0.95))
    )
    
    output$result_table <- renderTable({
      results
    }, rownames = TRUE)  # Mostrar nombres de filas si es necesario
    
    output$table_with_title <- renderUI({
      tagList(
        h4(paste("Tamaño Muestral ( Alpha:", round(alpha, 2), ")")),
        tableOutput("result_table")
      )
    })
    
    levels <- seq(0.80, 0.99, by = 0.01)
    mu_acc_values <- sapply(levels, function(level) mu.acc(len, v, rho, n0, level))
    mu_alc_values <- sapply(levels, function(level) mu.alc(len, v, rho, n0, level))
    mu_woc_90_values <- sapply(levels, function(level) mu.modwoc(len, v,  rho, n0, level, 0.90))
    mu_woc_95_values <- sapply(levels, function(level) mu.modwoc(len, v, rho, n0, level, 0.95))
    mu_freq_values <- sapply(levels, function(level) mu.freq(len, lambda, level))
    mu_bay_values <- sapply(levels, function(level) mu.freq(len, lambda, level)-n0)
    
    plot_data <- data.frame(
      level = levels,
      mu_acc = mu_acc_values,
      mu_alc = mu_alc_values,
      mu_woc_90 = mu_woc_90_values,
      mu_woc_95 = mu_woc_95_values,
      mu_freq = mu_freq_values,
      mu_bay = mu_bay_values
    )
    
    output$sample_size_plot <- renderPlot({
      ggplot(plot_data, aes(x = level)) +
        geom_line(aes(y = mu_acc, color = "ACC")) +
        geom_line(aes(y = mu_alc, color = "ALC")) +
        geom_line(aes(y = mu_woc_90, color = "WOC (90%)")) +
        geom_line(aes(y = mu_woc_95, color = "WOC (95%)")) +
        geom_line(aes(y = mu_freq, color = "Frecuentista")) +
        geom_line(aes(y = mu_bay, color = "Bayes (sigma conocido)")) +
        labs(title = "Comparación de todos los métodos",
             x = "1 - alpha",
             y = "Tamaño muestral",
             color = "Método") +
        theme_minimal()
    })
  })
}

# Ejecución de la aplicación Shiny
shinyApp(ui = ui, server = server)

