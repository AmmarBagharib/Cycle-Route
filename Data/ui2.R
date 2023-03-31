library(shiny)
library(ggplot2)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinycssloaders)

ui <- fluidPage(
  
  titlePanel("Tabsets"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Inputs excluded for brevity
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)