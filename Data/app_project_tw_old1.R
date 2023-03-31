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
library(summaryBox)
library(shinydashboard)

theme <- bslib::bs_theme(version = 4)

# define UI
ui <- fluidPage(
  titlePanel('Plan Your Route'), 
  
  theme = theme,
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId='origin', label=h3('Origin: Where you want to start your journey'), value = ""),
      hr(), 
      
      textInput(inputId='destination', label=h3('Destination: Where you want to stop your journey'), value = ""),
      hr(), 

      textInput(inputId='weight', label=h3('Your weight is (in kilograms)'), value = ""),
      hr(),
      
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
      hr()
     
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput(outputId = "map")),
        tabPanel("Plot", plotOutput(outputId = "plot")), 
        tabPanel("Summary", verbatimTextOutput(outputId = "summary"), 
        
        uiOutput("summarybox"))
        
      )
    )
  )
)

# define server logic
server <- function(input, output, session){
  
  key = 'AIzaSyAFwIr0aBkkLxXzihnK_LPlh-Ph77FZy44'
  
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
      placename <- paste(sep = "<br/>",
                         '<strong>', next_loc[closest.long.lat, "name"], '</strong>',
                         paste("Address:" ,next_loc[closest.long.lat, "vicinity"]),
                         paste0("Rating: ", next_loc[closest.long.lat, "rating"], "/5.0"))
      place_info <- list(placename,
                         as.numeric(closest.coordinate))
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
      print(matrix(unlist(data[2]), ncol = 2))
      print(class(data[3]))
      #print(origin)
      #print(test)
      #print(destination)
      doc <- mp_directions(origin = c(unlist(data[1])),
                           waypoints = matrix(unlist(data[2]), ncol = 2),
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
    
  output$summarybox <- renderUI(
      {
        data <- trigger()
        
        doc <- mp_directions(origin = c(unlist(data[1])),
                             waypoints = matrix(unlist(data[2]), ncol = 2),
                             destination = c(unlist(data[3])),
                             alternatives = F,
                             key = key,
                             mode = "bicycling",
                             quiet = F)   
        
        doc <- as_xml_document(doc)
        d <- mp_get_routes(doc)
        
        distance <- d$distance_m[1]
        duration <- input$duration*60
        weight <- input$weight
        speed <- distance/duration
        mph <- speed * 2.236936
        
        MET <- dplyr::case_when(mph < 10 ~ 4,
                                mph < 12 ~ 6.8,
                                mph < 14 ~ 8,
                                mph < 16 ~ 10,
                                mph >=16 ~ 12)
        print(MET)
        
        calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
        weightloss <- calories/7700
        
      fluidRow(
        summaryBox("Origin of Trip", input$origin, width = 3, icon = "fas fa-home", style = "info"),
        summaryBox("Destination of Trip", input$destination, width = 3, icon = "fas fa-torii-gate", style = "danger"),
        summaryBox("Distance of Trip", paste(d$distance_m[1], 'meters'), width = 3, icon = "fas fa-biking", style = "primary"),
        summaryBox("Duration of Trip", paste(input$duration, 'minutes'), width = 3, icon = "fas fa-clock", style = "success"),
        summaryBox("Speed of Trip", paste(round(speed, 2), 'meters/sec'), width = 3, icon = "fas fa-clock", style = "info"),
        summaryBox("Estimated Calories Burnt", paste(round(calories,2), "cals"), width = 3, icon = "fas fa-fire", style = "danger"), 
        summaryBox("Weight lost", paste(round(weightloss, 4), "kilograms"), width = 3, icon = "fas fa-calendar", style = "primary"),
        summaryBox("Amenities passed by", paste('need input'), width = 3, icon = "far fa-building", style = "info")
        )
      }
    )
}

shinyApp(ui = ui, server = server)





