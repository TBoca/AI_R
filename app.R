library(shiny)
library(tidyverse)
library(readxl)

# El archivo disponible es Rendimientos_decada.xlsx; "SD" (sin dato) pasa a NA
datos <- read_excel("Rendimientos_decada.xlsx", na = c("", "NA", "SD")) |>
  rename(sembrada = `Sup. Sembrada`, cosechada = `Sup. Cosechada`, campana = `Campaña`)

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cultivo", "Cultivo", choices = sort(unique(datos$Cultivo))),
      textOutput("faltantes")
    ),
    mainPanel(plotOutput("grafico"))
  )
)

server <- function(input, output) {
  datos_cultivo <- reactive({
    datos |> filter(Cultivo == input$cultivo)
  })

  output$faltantes <- renderText({
    n <- sum(is.na(datos_cultivo()$sembrada) | is.na(datos_cultivo()$cosechada))
    paste("Filas excluidas por valores faltantes:", n)
  })

  output$grafico <- renderPlot({
    datos_cultivo() |>
      drop_na(sembrada, cosechada) |>
      group_by(campana) |>
      summarise(Sembrada = sum(sembrada), Cosechada = sum(cosechada)) |>
      pivot_longer(-campana, names_to = "superficie", values_to = "hectareas") |>
      ggplot(aes(campana, hectareas, color = superficie, group = superficie)) +
      geom_line() +
      geom_point() +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
      labs(x = "Campaña", y = "Superficie (ha)", color = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
