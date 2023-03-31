library(shiny)
library(ggplot2)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinycssloaders)

# define UI
# define UI
ui <- fluidPage(
  
  titlePanel('Plan Your Route'),
  
  sidebarLayout(
    
    sidebarPanel(
      # first text
      textInput('origin', label=h3('Origin: Where you want to start your journey'), value = ""),
      hr(),
      
      # second text
      textInput('destination', label=h3('Destination: Where you want to stop your journey'), value = ""),
      hr(), 
      
      # slider bar
      sliderInput(inputId="duration", label="How long you plan to finish your journey (in mins)", value=60, min=10, max=600),
      
      # checkbox
      checkboxGroupInput('checkGroup', label=h3("Amenities you want to pass by"), 
                         choices = list("bus_station"="bus_station", "cafe"="cafe", "convenience_stores"="convenience_stores", "gas_stations"="gas_stations", "meal_takeaway"="meal_takeaway", 
                                        "park"="park", "shopping_mall"="shopping_mall", "tourist_attrction"="tourist_attraction", "train_station"="train_station"), 
                         selected="bus_station"),
      hr(), 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("origin"))
      )
    )
  )
)




# write a function to get the nearest location
nearest <- function(curr.long.lat, next.loc){ # curr.long.lat is a vector of the current location, next.loc is a character of the next target location e.g. "gas stations"
  # read csv of next.loc
  next_loc <- read.csv(gsub(" ", "", paste(gsub(" ", "_", next.loc), ".csv")))
  
  curr.long.latv <- s2_lnglat(curr.long.lat[1],
                              curr.long.lat[2])
  target.loc.long.lat <- s2_lnglat(next_loc$geometry.location.lng,
                                   next_loc$geometry.location.lat)
  closest.long.lat <- s2_closest_feature(curr.long.latv,
                                         target.loc.long.lat)
  closest.coordinate <- c(next_loc[closest.long.lat, "geometry.location.lng"],
                          next_loc[closest.long.lat, "geometry.location.lat"])
  
  placename <- next_loc[closest.long.lat, "name"]
  place_info <- list(placename, as.numeric(closest.coordinate))
  
  return (place_info) # returns a list with place name and a vector of the long and lat of the closest next location, to be supplied to the mp_directions function
}


nearest.loc.mat <- function(origin, ...){
  locations <- unlist(list(...))
  
  if (is.null(locations)){
    return(0) # placeholder for when no locations are supplied as input arguments; to be replaced with function to generate random route that fulfills other restraints
    
  } else {
    output <- list()
    output.rownames <- c()
    
    for (i in 1:length(locations)){
      inter <- c(origin[i, ])
      closest.amen <- nearest(inter, locations[i])
      closest.amen.long.lat <- closest.amen[2]
      closest.amen.name <- closest.amen[1]
      output.rownames <- c(output.rownames, closest.amen.name)
      output <- append(output, closest.amen.long.lat)
    }
  }
  
  output <- matrix(unlist(output),
                   ncol=2,
                   byrow=T)
  colnames(output) <- c("longitude", "latitude")
  rownames(output) <- output.rownames
  
  return (output)
}




intermediate.loc.mat <- function(origin, destination, ...){
  locations <- list(...)
  n <- length(locations)
  search_points <- gcIntermediate(origin, destination, n)
  output <- nearest.loc.mat(search_points, locations)
  return (output)
}



# define server logic
server <- function(input, output){
  
  key = 'AIzaSyAFwIr0aBkkLxXzihnK_LPlh-Ph77FZy44'
  
  # define origin
  #origin <- input$origin
  
  origin <- input$origin
  
  origin <- mp_geocode(origin, region='sg', postcode=NULL,
                       key=key, quiet=FALSE, timeout=10)
  origin <- mp_get_points(origin)
  origin <- c(origin$pnt$`1`[1], origin$pnt$`1`[2])
  
  
  # define destination
  destination<- input$destination
  destination <- mp_geocode(destination, region='sg', postcode=NULL,
                            key=key, quiet=FALSE, timeout=10)
  destination <- mp_get_points(destination)
  destination <- c(destination$pnt$`1`[1], destination$pnt$`1`[2])
  
  # define test, which includes origin, destination and intermediate points
  #test <- intermediate.loc.mat(origin, destination, "park", "convenience stores", "shopping mall")
  test <- intermediate.loc.mat(origin, destination, input$checkGroup)
  
  output$map <- renderLeaflet(
    {
      map <- leaflet() %>% addTiles()
      
      doc <- mp_directions(origin = origin,
                            waypoints = test,
                            destination = destination,
                            alternatives = T,
                            key = key,
                            mode = "bicycling",
                            quiet = F)   
      
      doc = as_xml_document(doc) 
      d <- mp_get_routes(doc) 
      
      # plot map
      map <- map %>% addPolylines(data = d, opacity = 1, weight = 4)
      
      map
    }
  )
  
 
  
  output$plot <- renderplotOutput(
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
      
    }
  )
}

shinyApp(ui = ui, server = server)





