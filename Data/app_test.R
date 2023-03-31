library(shiny)
library(ggplot2)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinycssloaders)

# define UI
ui <- fluidPage(
  
  titlePanel("Tabsets"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput('origin', label=h3('Origin: Where you want to start your journey'), value = ""),
      
      hr(),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("origin"))

      )
    )
  )
)


server <- function(input, output) {
  
  # You can access the value of the widget with input$text, e.g.
  output$origin <- input$origin 
  
}







shinyApp(ui = ui, server = server)