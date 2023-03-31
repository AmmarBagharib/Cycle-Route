# define UI
library(shiny)
library(ggplot2)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinycssloaders)


ui <- fuildPage(
  titlePanel('Plan your route'), 
  
  sidebarLayout(
    
    sidebarPanel(
      testInput('test', label=h3('Origin: Where you want to start your journey'), value = "Enter text"),
      hr(), 
      fuildRow(column(3, verbatimTextOutput("value")))
    ),
      
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Elevation", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("summary"))
        
      )
    )
  )
)
