library(s2)
library(geosphere)
library(dplyr)
library(osrm)
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
library(googleway)


setwd("/Users/harrischen/Desktop/NUS/Year 2 Sem 2/DBA3702/Project/Data Files")
bus_station <- read.csv('bus_station.csv')

# define UI
ui <- fluidPage(
  titlePanel('Plan Your Route'), 
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId='origin', label=h3('Origin: Where you want to start your journey'), value = ""),
      hr(), 
      #fluidRow(column(3, verbatimTextOutput("value"))), 
      
      textInput(inputId='destination', label=h3('Destination: Where you want to stop your journey'), value = ""),
      hr(), 
      #fluidRow(column(3, verbatimTextOutput("value"))), 
      
      sliderInput(inputId="duration", label="How long you plan to finish your journey (in mins)", value=60, min=10, max=600),
      
      checkboxGroupInput(inputId='checkGroup', label=h3("Amenities you want to visit en route"), 
                         choices = list("bus_station"="bus_station",
                                        "cafe"="cafe",
                                        "convenience_stores"="convenience_stores",
                                        "gas_stations"="gas_stations",
                                        "meal_takeaway"="meal_takeaway", 
                                        "park"="park",
                                        "shopping_mall"="shopping_mall",
                                        "tourist_attrction"="tourist_attraction",
                                        "train_station"="train_station"), 
                         selected = "bus_station"),
      hr(), 
      #fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput(outputId = "map")),
        tabPanel("Plot", plotOutput(outputId = "plot")), 
        tabPanel("Summary", verbatimTextOutput(outputId = "summary"))
        
      )
    )
  )
)

# define server logic
server <- function(input, output){
  
  key = 'AIzaSyDND8XIlLvVZfxB05cdYGm7PtgncMLqMrU'
  
  trigger <- reactive({
    origin <- input$origin
    destination <- input$destination
    checkGroup <- input$checkGroup
    
    # write a function to get the nearest location
    nearest <- function(curr.long.lat, next.loc){ # curr.long.lat is a vector of the current location, next.loc is a character of the next target location e.g. "gas stations"
      # read csv of next.loc
      next_loc <- read.csv(paste("/Users/harrischen/Desktop/NUS/Year 2 Sem 2/DBA3702/Project/Data Files/", gsub(" ", "", paste(gsub(" ", "_", next.loc), ".csv")), sep = ""))
      print("current.long.lat")
      print(curr.long.lat)
      curr.long.latv <- s2_lnglat(curr.long.lat[1],
                                  curr.long.lat[2])
      target.loc.long.lat <- s2_lnglat(next_loc$geometry.location.lng,
                                       next_loc$geometry.location.lat)
      closest.long.lat <- s2_closest_feature(curr.long.latv,
                                             target.loc.long.lat)
      closest.coordinate <- c(next_loc[closest.long.lat, "geometry.location.lng"],
                              next_loc[closest.long.lat, "geometry.location.lat"])
      print("closest coordinate")
      print(closest.coordinate)
      placename <- next_loc[closest.long.lat, "name"]
      place_info <- list(placename, as.numeric(closest.coordinate))
      print("place_info")
      print(place_info)
      return (place_info) # returns a list with place name and a vector of the long and lat of the closest next location, to be supplied to the mp_directions function
    }
    
    
    nearest.loc.mat <- function(origin, ...){
      locations <- unlist(list(...))
      print(origin)
      if (is.null(locations)){
        return(0) # placeholder for when no locations are supplied as input arguments; to be replaced with function to generate random route that fulfills other restraints
        
      } else {
        output <- list()
        output.rownames <- c()
        
        for (i in 1:length(locations)){
          print("i")
          print(i)
          print("origin in for loop")
          print(origin)
          inter <- c(origin[i, ])
          print("inter")
          print(inter)
          print("locations")
          print(locations)
          closest.amen <- nearest(inter, locations[i])
          closest.amen.long.lat <- closest.amen[2]
          closest.amen.name <- closest.amen[1]
          output.rownames <- c(output.rownames, closest.amen.name)
          output <- append(output, closest.amen.long.lat)
          print("output in for loop of nearest.loc.mat")
          print(output)
        }
      }
      
      output <- matrix(unlist(output),
                       ncol=2,
                       byrow = T)
      
      colnames(output) <- c("longitude", "latitude")
      rownames(output) <- output.rownames
      
      return (output)
    }
    
    intermediate.loc.mat <- function(origin, destination, ...){
      locations <- list(...)
      print("intermediatelocmatlocation")
      print(locations)
      n <- length(unlist(locations))
      print("n")
      print(n)
      search_points <- gcIntermediate(origin, destination, n)
      print("search points")
      print(search_points)
      output <- nearest.loc.mat(search_points, locations)
      print("output")
      print(output)
      return (output)
    }
    
    # define origin
    origin <- mp_geocode(origin,
                         region='sg',
                         postcode=NULL,
                         key=key,
                         quiet=FALSE,
                         timeout=10)
    origin <- mp_get_points(origin)
    origin <- c(origin$pnt$`1`[1], origin$pnt$`1`[2])
    
    # define destination
    destination <- mp_geocode(destination,
                              region='sg',
                              postcode=NULL,
                              key=key,
                              quiet=FALSE,
                              timeout=10)
    destination <- mp_get_points(destination)
    destination <- c(destination$pnt$`1`[1], destination$pnt$`1`[2])
    
    print("Checkgroup")
    print(checkGroup)
    print("destination")
    print(destination)
    print("Origin")
    print(origin)
    
    # define test, which includes origin, destination and intermediate points
    # test <- intermediate.loc.mat(origin, destination, "park", "convenience stores", "shopping mall")
    test <- intermediate.loc.mat(origin, destination, checkGroup)
    print("test")
    print(test)
    test1 <- list(origin, test, destination)
  })
  
  output$map <- renderLeaflet(
    {
      m <- leaflet() %>%
        addTiles()
      
      data <- trigger()
      print("data")
      print(class(data[1]))
      print(class(data[2]))
      print(class(data[3]))
      #print(origin)
      #print(test)
      #print(destination)
      doc <- mp_directions(origin = c(unlist(data[1])),
                           waypoints = c(unlist(data[2])),
                           destination = c(unlist(data[3])),
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)   
      
      doc <- as_xml_document(doc) 
      d <- mp_get_routes(doc)
      
      # plot map
      m <- addPolylines(m, data = d, opacity = 1, weight = 4)
      
      return (m)
    }
  )
  
  output$plot <- renderPlot(
    {
      df <- google_elevation(df_locations = data.frame(rbind(rbind(origin, test), destination)),
                             location_type = "path",
                             samples = 100,
                             simplify = TRUE,
                             key = key)
      
      
      df_plot <- data.frame(elevation = df$results$elevation,
                            location = as.integer(rownames(df$results)))
      
      ggplot(data = df_plot, aes(x = location, y = elevation)) +
        geom_line()
      
      return (df_plot)
    }
  )
}

shinyApp(ui = ui, server = server)





