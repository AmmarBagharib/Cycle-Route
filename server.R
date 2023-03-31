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

googleKey <- 'insert google key here'

# define server logic
server <- function(input, output, session){
  
  m <- leaflet() %>%
    addTiles()
  
  key <- googleKey
  
  trigger.map.and.elevation <- reactive({
    # This function returns a list of 3 objects:
    # 1. waypoints # for summary tab
    # 2. final.map # for main leaflet plot
    # 3. route_df # for elevation plot
    
    origin.input <- input$origin
    destination.input <- input$destination
    checkGroup.input <- input$checkGroup

    # Test
    # origin.input <- 'NUS'
    # destination.input <- 'Tampines Mall'
    # checkGroup.input <- 'cafe'
    # next.loc <- "Meal Takeaway"
    
    # function to get the nearest location when supplied a vector of long and lat
    nearest <- function(curr.long.lat, next.loc){ # curr.long.lat is a vector of the current location, next.loc is a character of the next target location e.g. "gas stations"
      
      # read csv of next.loc
      next_loc <- read.csv(
        here::here("Data", paste0(gsub(" ", "_", tolower(next.loc)), ".csv"))
      )
      
      # get the long and lat of closest point identified
      curr.long.latv <- s2_lnglat(curr.long.lat[1],
                                  curr.long.lat[2]) 
      target.loc.long.lat <- s2_lnglat(next_loc$geometry.location.lng,
                                       next_loc$geometry.location.lat)
      closest.long.lat <- s2_closest_feature(curr.long.latv,
                                             target.loc.long.lat)
      closest.coordinate <- c(next_loc[closest.long.lat, "geometry.location.lng"],
                              next_loc[closest.long.lat, "geometry.location.lat"])
      
      #print("closest coordinate in nearest")
      #print(closest.coordinate)

      # get the name and details of the place
      placedetails <- paste(sep = "<br/>",
                            '<strong>', next_loc[closest.long.lat, "name"], '</strong>',
                            paste("Address:" ,next_loc[closest.long.lat, "vicinity"]),
                            paste0("Rating: ", next_loc[closest.long.lat, "rating"], "/5.0"))
      
      place_info <- list(placedetails,
                         as.numeric(closest.coordinate))

      return (place_info) # returns a list with place name and a vector of the long and lat of the closest next location, to be supplied to the mp_directions function
    }
    # print("nearest is run")
    
    # create matrix of intermediate locations
    nearest.loc.mat <- function(origin, ...){
      
      locations <- unlist(list(...))
      output <- list()
      output.rownames <- c()
      
      if (length(locations) != 0){
        
        for (i in 1:length(locations)){

          inter <- c(origin[i, ])
          closest.amen <- nearest(inter, locations[i])
          closest.amen.long.lat <- closest.amen[2]
          closest.amen.name <- closest.amen[1]
          output.rownames <- c(output.rownames, closest.amen.name)
          output <- append(output, closest.amen.long.lat)

        }
        
        output <- matrix(unlist(output),
                         ncol = 2,
                         byrow = T)
        
        colnames(output) <- c("longitude", "latitude")
        rownames(output) <- output.rownames
        
      } else {
        
        output <- 0
        
      }
      
      return (output)
    }
    # print("nearest.loc.mat is run")
    
    # final run function
    intermediate.loc.mat <- function(origin, destination, ...){
      
      locations <- list(...)
      n <- length(unlist(locations))
      search_points <- gcIntermediate(origin, destination, n)
      output <- nearest.loc.mat(search_points, locations)

      return (output)
    }
    # print("intermediate.loc.mat is run")
    
    # define origin
    origin <- mp_geocode(origin.input,
                         region = 'sg',
                         postcode = NULL,
                         key = key,
                         quiet = F,
                         timeout = 10)
    
    origin <- mp_get_points(origin)
    origin <- c(origin$pnt$`1`[1], origin$pnt$`1`[2])
    #print("origin is run")
    
    # define destination
    destination <- mp_geocode(destination.input,
                              region = 'sg',
                              postcode = NULL,
                              key = key,
                              quiet = F,
                              timeout = 10)
    #print("destination is run")
    
    destination <- mp_get_points(destination)
    destination <- c(destination$pnt$`1`[1], destination$pnt$`1`[2])
    
    # define intermediate points
    if (is.null(origin) | is.null(destination)){
      
      waypoints <- 0
      
    } else {
      
      waypoints <- intermediate.loc.mat(origin, destination, checkGroup.input)
  
    }
    #print("first if is run")
    
    # optimise route based on identified points
    if (is.null(origin) | is.null(destination)){
      
      d <- 0
      
    } else if (length(checkGroup.input) == 0){
      
      doc <- mp_directions(origin = origin,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
      segments <- mp_get_segments(doc)
      waypoints <- 0
      
    } else {
      
      #print("waypoints in else")
      print(waypoints)
      doc <- mp_directions(origin = origin,
                           waypoints = waypoints,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
      segments <- mp_get_segments(doc)
      
    }
    # print("second if is run")
    
    # plot route on leaflet
    if (is.null(origin) | is.null(destination)){
      
      m <- 0
      
    } else {

      instructions <- data.frame(segments$instructions)
      coords <- data.frame(st_coordinates(segments$geometry)) %>% group_by(L1) %>% slice_head()
      instructions.final <- cbind(coords, instructions) %>% select(-L1)
      
      m <- addPolylines(m,
                        data = d,
                        opacity = 1, weight = 4, color = '#1362e8') %>%
        addCircleMarkers(data = instructions.final,
                         lng = ~X, lat = ~Y,
                         popup = ~segments.instructions,
                         color = "#8b0000", radius = 5)
    }
    # print("third if is run")
    
    # add markers
    addLocationMarkers <- function(map, origin.v, origin.c, waypoints.m, destination.v, destination.c){
      
      iconSet <- awesomeIconList(
        home = makeAwesomeIcon(text = fa("home"), iconColor = "white", markerColor = "green"),
        dest = makeAwesomeIcon(text = fa("circle"), iconColor = "white", markerColor = "green"),
        cafe = makeAwesomeIcon(icon = 'coffee', library = 'ion', iconColor = 'white', markerColor = 'brown'),
        meal_takeaway = makeAwesomeIcon(icon = 'cutlery', library = 'glyphicon', iconColor = 'rgb(192, 255, 0)', markerColor = 'darkpurple'),
        convenience_stores = makeAwesomeIcon(icon = 'cart-arrow-down', library = 'fa', iconColor = 'white', markerColor = 'green'),
        park = makeAwesomeIcon(icon = 'tree', library = 'fa', iconColor = 'purple', markerColor = 'yellow'),
        shopping_mall = makeAwesomeIcon(icon = 'shopping-cart', library = 'fa', iconColor = 'white', markerColor = 'pink'),
        tourist_attraction = makeAwesomeIcon(icon = 'industry', library = 'fa', iconColor = 'white', markerColor = 'yellow'),
        gas_stations = makeAwesomeIcon(icon = 'fire', library = 'fa', iconColor = 'black', markerColor = 'orange'),
        train_station = makeAwesomeIcon(icon = 'train', library = 'fa', iconColor = 'white', markerColor = 'darkpurple')
      )
      
      if (length(waypoints.m) == 1){
        
        m <- rbind(origin.v,
                   destination.v)

        colnames(m) <- c("longitude", "latitude")
        
      } else {
        
        m <- rbind(rbind(origin.v,
                         waypoints.m),
                   destination.v)
        
      }
      
      # change the row names of origin and destination
      rownames(m)[1] <- paste0('<strong>', "Start: ", '</strong>',
                               origin.c)
      rownames(m)[nrow(m)] <- paste0('<strong>', "Destination: ", '</strong>',
                                     destination.c)
      
      m <- asplit(m, 2)
      
      longitudes <- m$longitude
      latitudes <- m$latitude
      
      for (i in 1:length(longitudes)){
        if (i == 1){
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet["home"])
          
        } else if (i == length(longitudes)) {
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet["dest"])
          
        } else {
          
          map <- map %>%
            addAwesomeMarkers(lng = longitudes[i],
                              lat = latitudes[i],
                              popup = names(longitudes[i]),
                              icon = iconSet[checkGroup.input[i - 1]])
        }
      }
      
      return (map)
    }
    #print("addLocationMarkers is run")
    
    route_points <- function(doc){
      #this function takes in a xml doc of a route
      segments <- mp_get_segments(doc)
      distances <- segments$distance_m
      
      segments_df <- as.data.frame(st_coordinates(segments$geometry))
      
      
      segments_dist_df <- segments_df %>% group_by(L1) %>% summarise(Count = n())
      
      segments_dist_df <- bind_cols(segments_dist_df, distances)
      
      colnames(segments_dist_df) <- c("L1", "Count", "Cumulative_Distance")
      
      segments_dist_df$indiv_dist <- round(segments_dist_df$Cumulative_Distance / segments_dist_df$Count, 2)
      
      route_points <- as.data.frame(st_coordinates(segments$geometry))
      
      route_points <- left_join(route_points, segments_dist_df, by = 'L1') %>% 
        mutate(Total_Distance_metres = round(cumsum(indiv_dist), 0))
      
      colnames(route_points) <- c("lon", "lat", "L1", "Count", "Cumulative_Distance", "Indiv_Dist", "Total_Distance_metres")
      
      return(route_points)
    }
    # print("route_points is run")
    
    # Elevation function
    elevation_fn <- function(df){
      # df MUST have columns with Latitude and Longitude column names!!!
      # df <- route_points
      # check for empty df
      # df <- route_points
      if (nrow(df) == 0){
        
        return(FALSE)
        
      } else {
        
        df <- df[, c("lon", "lat")] 
        elevation_df <- data.frame() #empty df
        
        #google elevation takes in max 500 points
        #find the number of times we need to get 500 elevations
        
        count <- ceiling(nrow(df) / 200)
        nrow_initial_df <- nrow(df)
        initial_row <- 1
        max_row <- ifelse(nrow(df) >= 200, 200, nrow(df))
        
        for (i in 1:count){
          
          rows_to_elevate <- df[initial_row:max_row, ]
          df_sliced <- df[(max_row + 1):nrow(df), ]
          
          elevation_data <- google_elevation(df_locations = rows_to_elevate,
                                             location_type = "path",
                                             samples = nrow(rows_to_elevate),
                                             simplify = T,
                                             key = key)
          Sys.sleep(0.1)
          
          elevation_to_bind <- as.data.frame(round(elevation_data$results$elevation, 1))
          elevation_df <- bind_rows(elevation_df, elevation_to_bind)
          
          initial_row <- max_row + 1
          max_row <- max_row + 200
          max_row <- ifelse(max_row >= nrow_initial_df,
                            nrow_initial_df,
                            max_row)
          
          i <- i + 1
        }
      }
      
      elevation_df$point_number <- seq.int(nrow(elevation_df)) # creates a column of row numbers in the df
      return(elevation_df)
    }
    # print("elevation_fn is run")
    
    # functions to derive route df and main df.
    # route df is used to plot elevation data,
    # main df is used to plot geom_points on elevation data.
    
    # function to get df used to plot elevation
    route_df_to_plot <- function(route_points){
      
      #this function takes in the route_points df and returns the df that should be used for plotting
      route_points$point_number <- seq.int(nrow(route_points)) #return row number in a column
      elevation_df <- elevation_fn(route_points)
      Sys.sleep(0.1)
      route_df <- inner_join(elevation_df,
                             route_points,
                             by = 'point_number')
      #route_df <- left_join(route_df, main_points, by='point_number')
      colnames(route_df) <- c("elevation", "point_number", "lon", "lat", "L1", "Count", 
                              "Cumulative_Distance", "Indiv_Dist", "Total_Distance_metres")  
      
      return(route_df)
    }
    # print("route_df_to_plot is run")
    
    # function to get df used to plot main names
    main_df_to_plot <- function(route_df, origin, intermediates, destination){
      if (length(intermediates) == 0){
        
        main_points <- data.frame(rbind(origin,
                                        destination))
        main_points$name <- rownames(main_points)
        point_number_vector <- c(1, nrow(route_df))
        
      } else {
        
        main_points <- data.frame(rbind(rbind(origin,
                                              intermediates),
                                        destination))
        main_points$name <- rownames(main_points)
        point_number_vector <- c()
        route_df.lnglat <- s2_lnglat(route_df$lon, route_df$lat) # convert points to s2 object
        
        for (i in 1:nrow(main_points)){
          
          main.point.to.search <- s2_lnglat(main_points$lon[[i]], main_points$lat[[i]])
          point_number <- as.integer(s2_closest_feature(main.point.to.search, route_df.lnglat))
          point_number_vector <- append(point_number_vector, point_number)
          
        }
      }
      
      main_points <- bind_cols(main_points, point_number_vector)
      colnames(main_points) <- c("lon", "lat", "name", "point_number")
      main_points <- left_join(main_points, route_df, by = 'point_number')
      
      return(main_points)
    }
    # print("main_df_to_plot is run")
    
    if (is.null(origin) | is.null(destination)){
      
      route_df <- 0
      print("origin and destination is null")
      
    } else {
      route_points <- route_points(doc)
      route_df <- route_df_to_plot(route_points)
      main_df <- main_df_to_plot(route_df, origin, waypoints, destination)
      main_df[1, 'Total_Distance_metres'] <- 0 # set origin distance to 0
      
      route_df <- left_join(route_df,
                            main_df[, c('point_number', 'name')],
                            by = "point_number")
      route_df[is.na(route_df$name), ]$name <- ''
      
      route_df[1, ]$Total_Distance_metres <- 0
      
      slider_formatter <- function(x){
        dplyr::case_when(
          x < 1e3 ~ paste0(as.character(x), "M"),
          x < 1e5 ~ paste0(as.character(round(x/1e3, 1)), "KM"),
          TRUE ~ paste0(as.character(round(x/1e3)), "KM")
        )
        
        #print("route df done")
      }
      
      route_df <- route_df %>% mutate(levels_formatted = slider_formatter(Total_Distance_metres))
      # print("route df finalised")
    }
    
    # print("fourth if is run")
    
    if (is.null(origin) | is.null(destination)){
      final.map <- 0
    } else {
      final.map <- addLocationMarkers(m, origin, origin.input, waypoints, destination, destination.input)
    }
    
    # print("final.map is run")
    
    plot_list <- list(waypoints, # for summary
                      final.map, # for main leaflet plot
                      route_df # for elevation plot
    )
    
    # print("plot_list is run")
    return (plot_list)
    
    }
  )
  # map
  output$map <- renderLeaflet(
    {
      if (input$origin == "" | input$destination == ""){
        
        m <- leaflet() %>% setView(lat = 1.356660, lng = 103.835381,
                                   zoom = 12) %>% 
          addTiles() %>%
          addMarkers(lat = 1.356660, lng = 103.835381, label = "Singapore")
        
      } else {
        
        main.map <- trigger.map.and.elevation()
        
        m <- main.map[[2]]
        
        if (is.numeric(m)){
          
          m <- leaflet() %>% setView(lat = 1.356660, lng = 103.835381,
                                     zoom = 12) %>% 
            addTiles() %>%
            addMarkers(lat = 1.356660, lng = 103.835381, label = "Singapore")

        }
      }
      
      return (m)
    }
  )
  
  
  output$plot <- renderPlotly(
    {
      if (input$origin == "" | input$destination == ""){
        
        fig <- plotly_empty() %>%
          layout(title = "Please input your starting point and destination!",
                 plot_bgcolor = "#ECF0F5",
                 paper_bgcolor = "#ECF0F5")
        
        return (fig)
        
      } else {
        
        accumulate_by <- function(dat, var) {
          var <- lazyeval::f_eval(var, dat)
          lvls <- plotly:::getLevels(var)
          dats <- lapply(seq_along(lvls),
                         function(x) { cbind(dat[var %in% lvls[seq(1, x)], ],
                                             frame = lvls[[x]])
                         }
          )
          
          dplyr::bind_rows(dats)
          
        }
        
        route_df <- trigger.map.and.elevation()[[3]]
        
        if (class(route_df) == "numeric"){
          
          print(route_df)
          fig <- plotly_empty() %>%
            layout(title = "Please input valid place names!",
                   plot_bgcolor = "#ECF0F5",
                   paper_bgcolor = "#ECF0F5")
          
          return (fig)
          
        } else {
          
          fig <- plot_ly(data = route_df,
                         x = ~Total_Distance_metres, 
                         y = ~elevation, 
                         type = 'scatter', 
                         mode = 'lines', 
                         line = list(color = 'rgba(255, 168, 108, 1)'),
                         fill = 'tozeroy',
                         fillcolor = list(color = 'rgba(255, 168, 108, 0.3)'),
                         text = ~levels_formatted, 
                         hovertemplate = paste(
                           "<b>Distance</b>: %{x}m <br>",
                           "<b>Elevation</b>: %{y}m <br>",
                           "<extra></extra>")) %>%
            layout(hovermode = "x unified",
                   plot_bgcolor = "#ECF0F5",
                   paper_bgcolor = "#ECF0F5")
          
          fig <- fig %>%
            layout(title = "Elevation along your journey",
                   yaxis = list(title = "Elevation", 
                                zeroline = F,
                                tickprefix = ""),
                   xaxis = list(title = "Distance", 
                                tickprefix = "",
                                zeroline = F, 
                                showgrid = F
                   )
            )
          
          fig %>% animation_opts(frame = 100, 
                                 transition = 0, 
                                 redraw = F
          )
          
          return (fig)
          
          }
      }
    }
  )
  
  output$Instruction <- renderInfoBox(
    {       
      fluidRow(
        infoBox("Instructions:", 
                value ="Click the red circles along your route for to view route instructions!",
                width = 12, color = "navy", icon=icon("book", lib="glyphicon"))
      )
    }
  )
  
  output$heythere <- renderInfoBox({
    infoBox("Heythere", "Change the duration of your trip if you'd like to!",color = "navy", icon=icon("edit", lib="glyphicon"))
    #print("heythere rendered")
  })
  
  output$origin <- renderInfoBox({
    infoBox("Origin of Trip",input$origin, color = "light-blue", icon=icon("map-marker", lib="glyphicon"))
    #print("origin of trip rendered")
  })
  
  output$destination <- renderInfoBox({
    infoBox("Destination of Trip",input$destination, color = "light-blue", icon=icon("flag", lib="glyphicon"))
    #print("destination rendered")
  })
  
  output$distance <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    
    infoBox("Distance of Trip",ifelse(distance>1000, paste(distance/1000, 'km'), paste(distance, 'meters')), 
            color = "green", icon=icon("transfer", lib="glyphicon"))
    print("distance rendered")
  })
  
  output$duration <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    
    duration <- (input$NumHours*60 + input$NumMins)*60
    
    infoBox("Duration of Trip",paste(input$NumHours, 'Hours', input$NumMins, 'Minutes'), color = "green", icon=icon("time", lib="glyphicon"))
    print("duration rendered")
  })
  
  output$speed <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    duration <- (input$NumHours*60 + input$NumMins)*60

    speed <- distance/duration
    
    #recommended_speed <- p(p(paste(round(speed * 3.6, 2), 'Km/hr')))
    recommended_speed <- paste(round(speed * 3.6, 2), 'Km/hr')

    tagList(infoBox(title="Recommended Speed for Your Trip", value=recommended_speed, color = "teal", icon=shiny::icon("forward", lib="glyphicon")))
    #print("speed rendered")
  })
  
  output$calories <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    duration <- (input$NumHours*60 + input$NumMins)*60
    print(input$NumHours)
    #print(input$NumMins)
    #print(paste0("duration: ", duration))
    
    weight <- input$weight
    #print(paste0("weight: ", weight))
    
    speed <- distance/duration
    
    mph <- speed * 2.236936
    
    MET <- dplyr::case_when(mph < 10 ~ 4,
                            mph < 12 ~ 6.8,
                            mph < 14 ~ 8,
                            mph < 16 ~ 10,
                            mph >= 16 ~ 12)
    
    calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
    #print(paste0("calories: ", calories))
    
    calories_burnt <- paste(round(calories,0), "cals")
    
    infoBox("Estimated Calories Burnt", calories_burnt, color = "teal", icon=icon("fire", lib="glyphicon"))
  })
  
  
  output$weight <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    route_df <- data[[3]]
    # duration <- 30
    # weight <- 80
    distance <- route_df[nrow(route_df), "Total_Distance_metres"]
    #print(paste0("distance: ", distance))
    
    duration <- (input$NumHours*60 + input$NumMins)*60
    #print(paste0("duration: ", duration))
    
    weight <- input$weight
    speed <- distance/duration
    mph <- speed * 2.236936
    
    MET <- dplyr::case_when(mph < 10 ~ 4,
                            mph < 12 ~ 6.8,
                            mph < 14 ~ 8,
                            mph < 16 ~ 10,
                            mph >= 16 ~ 12)
    
    calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
    weightloss <- calories/7700
    #print(paste0("weightloss: ", weightloss))
    
    infoBox("Weight lost", paste(round(weightloss, 3), "kilograms"), color = "light-blue", icon=icon("scale", lib="glyphicon"))
    
    print("Weigth lost class is: ")
    print(class(infoBox("Weight lost", paste(round(weightloss, 3), "kilograms"), color = "light-blue", icon=icon("scale", lib="glyphicon"))))
  })
  
  
  output$amenities <- renderInfoBox({
    data <- trigger.map.and.elevation()
    way.points <- data[[1]]
    
    amenities <- strsplit(rownames(way.points), split = "<br/>")
    #amenities
    amenity.c <- ''
    
    for (amenity in amenities){
      #print(amenity)
      
      #print(amenity[[2]])
      
      amenity.c <- p(amenity.c,
                     p(amenity[[2]]))
    }
    #print(amenity.c)
    infoBox("Amenities passed by", amenity.c, width=NULL, color = "green", icon=icon("screenshot", lib="glyphicon"))
    #print("amenities rendered")
  })

}

