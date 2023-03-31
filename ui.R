library(s2)
library(geosphere)
library(ggmap)
library(shiny)
library(shinyTime)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)
library(mapsapi)
library(xml2)
library(summaryBox)
library(plotly)
library(googleway)
library(sf)
library(XML)
library(tidyr)
library(curl)
library(rvest)
library(dplyr)
library(stringr)
library(png)
library(patchwork)
library(egg)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
library(shinydashboard)
library(here)

# define UI
ui <- dashboardPage(skin='red', 
                    dashboardHeader(title="Cycle Route SG"), 
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Map and Route Details', tabName='map', icon=icon("globe", lib = "glyphicon")), 
                        menuItem('Summary', tabName='summary', icon=icon("signal", lib = "glyphicon"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # Tab 1: map and elevation
                        tabItem(tabName = 'map', 
                                fluidRow(
                                  column(width=8, 
                                         leafletOutput(outputId = "map", height=500), 
                                         br(),
                                         #
                                         infoBoxOutput(outputId = "Instruction", width=12)), 
                                  column(width=4, 
                                         box(#width=NULL, height=NULL, 
                                           textInput(inputId = 'origin', label = h3('Origin: Where you want to start your journey'), value = ""),
                                           hr(), 
                                           
                                           textInput(inputId = 'destination', label = h3('Destination: Where you want to stop your journey'), value = ""),
                                           hr(), 
                                           
                                           checkboxGroupInput(inputId = 'checkGroup', label = h3("Amenities you want to visit en route (pick at least 1!)"), 
                                                              choices = list(
                                                                "Cafe"="cafe",
                                                                "Convenience Stores"="convenience_stores",
                                                                "Gas Stations"="gas_stations",
                                                                "Meal Takeaway"="meal_takeaway", 
                                                                "Park"="park",
                                                                "Shopping Mall"="shopping_mall",
                                                                "Tourist Attraction"="tourist_attraction"), 
                                           ),
                                           hr(),
                                           
                                           h3("How long you plan to finish your journey (in hours & mins)"),
                                           fluidRow(
                                             column(5,
                                                    numericInput(inputId = "NumHours", label = "No. of Hours", value = 0, min = 0, max = 24)),
                                             column(5,
                                                    numericInput(inputId = "NumMins", label = "No. of Minutes", value = 0, min = 0, max = 59))),
                                           textInput(inputId = 'weight', label = h3('Your weight is (in kilograms)'), value = ""), submitButton("Confirm"),
                                           hr(),
                                           
                                           width = 12)
                                  ))
                        ),
                        
                        # Tab 2: summary
                        tabItem(tabName='summary', 
                                fluidRow(
                                  infoBoxOutput(outputId = 'heythere', width=12),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  plotlyOutput(outputId = "plot"),
                                  br(),
                                  infoBoxOutput(outputId = "origin"),
                                  #infoBoxOutput(outputId = "distance"),
                                  #infoBoxOutput(outputId = "speed"),
                                  infoBoxOutput(outputId = "destination"),
                                  #infoBoxOutput(outputId = "duration"),
                                  infoBoxOutput(outputId = "calories"),
                                  #infoBoxOutput(outputId = "weight"),
                                  infoBoxOutput(outputId = "amenities", width=12)))
                      )
                    )
)