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

theme <- bslib::bs_theme(version = 4)

# define UI
ui <- fluidPage(
  titlePanel('Plan Your Route'), 
  
  theme = theme,
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId = 'origin', label = h3('Origin: Where you want to start your journey'), value = ""),
      hr(), 
      
      textInput(inputId = 'destination', label = h3('Destination: Where you want to stop your journey'), value = ""),
      hr(), 
      
      textInput(inputId = 'weight', label = h3('Your weight is (in kilograms)'), value = ""),
      hr(),
      
      h3("How long you plan to finish your journey (in hours & mins)"),
      fluidRow(
        column(5,
               numericInput(inputId = "NumHours", label = "No. of Hours", value = 0, min = 0, max = 24)),
        column(5,
               numericInput(inputId = "NumMins", label = "No. of Minutes", value = 0, min = 0, max = 59))),
      
      checkboxGroupInput(inputId = 'checkGroup', label = h3("Amenities you want to visit en route"), 
                         choices = list("bus_station"="bus_station",
                                        "cafe"="cafe",
                                        "convenience_stores"="convenience_stores",
                                        "gas_stations"="gas_stations",
                                        "meal_takeaway"="meal_takeaway", 
                                        "park"="park",
                                        "shopping_mall"="shopping_mall",
                                        "tourist_attraction"="tourist_attraction",
                                        "train_station"="train_station"), 
                         selected = "bus_station"),
      hr(),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map and Route Details", 
                 leafletOutput(outputId = "map"), 
                 plotlyOutput(outputId = "plot"),
                 uiOutput(outputId = "Instruction")),
        tabPanel("Summary", uiOutput(outputId = "summarybox")),
        tabPanel("Weather Forecast", 
                 plotOutput(outputId = "uv"),
                 plotOutput(outputId = 'temperature'), 
                 plotOutput(outputId = 'humid.rain'))
        
      ), width = 9)
  )
)

# define server logic
server <- function(input, output){
  
  m <- leaflet() %>%
    addTiles()
  
  key = 'AIzaSyDND8XIlLvVZfxB05cdYGm7PtgncMLqMrU'
  
  trigger.map.and.elevation <- reactive({
    
    origin.input <- input$origin
    destination.input <- input$destination
    checkGroup.input <- input$checkGroup
    
    # function to get the nearest location when supplied a vector of long and lat
    nearest <- function(curr.long.lat, next.loc){ # curr.long.lat is a vector of the current location, next.loc is a character of the next target location e.g. "gas stations"
      
      # read csv of next.loc
      next_loc <- read.csv(paste0(getwd(),
                                  "/Data Files/",
                                  gsub(" ",
                                       "",
                                       paste(gsub(" ",
                                                  "_",
                                                  next.loc),
                                             ".csv")),
                                  sep = ""))
      #print("current.long.lat")
      #print(curr.long.lat)
      # get the long and lat of closest point identified
      curr.long.latv <- s2_lnglat(curr.long.lat[1],
                                  curr.long.lat[2]) 
      target.loc.long.lat <- s2_lnglat(next_loc$geometry.location.lng,
                                       next_loc$geometry.location.lat)
      closest.long.lat <- s2_closest_feature(curr.long.latv,
                                             target.loc.long.lat)
      closest.coordinate <- c(next_loc[closest.long.lat, "geometry.location.lng"],
                              next_loc[closest.long.lat, "geometry.location.lat"])
      #print("closest coordinate")
      #print(closest.coordinate)
      # get the name and details of the place
      placedetails <- paste(sep = "<br/>",
                            '<strong>', next_loc[closest.long.lat, "name"], '</strong>',
                            paste("Address:" ,next_loc[closest.long.lat, "vicinity"]),
                            paste0("Rating: ", next_loc[closest.long.lat, "rating"], "/5.0"))
      
      place_info <- list(placedetails,
                         as.numeric(closest.coordinate))
      #print("place_info")
      #print(place_info)
      return (place_info) # returns a list with place name and a vector of the long and lat of the closest next location, to be supplied to the mp_directions function
    }
    
    # create matrix of intermediate locations
    nearest.loc.mat <- function(origin, ...){
      locations <- unlist(list(...))
      output <- list()
      output.rownames <- c()
      
      for (i in 1:length(locations)){
        #print("i")
        #print(i)
        #print("origin in for loop")
        #print(origin)
        inter <- c(origin[i, ])
        #print("inter")
        #print(inter)
        #print("locations")
        #print(locations)
        closest.amen <- nearest(inter, locations[i])
        closest.amen.long.lat <- closest.amen[2]
        closest.amen.name <- closest.amen[1]
        output.rownames <- c(output.rownames, closest.amen.name)
        output <- append(output, closest.amen.long.lat)
        #print("output in for loop of nearest.loc.mat")
        #print(output)
      }
      
      output <- matrix(unlist(output),
                       ncol = 2,
                       byrow = T)
      
      colnames(output) <- c("longitude", "latitude")
      rownames(output) <- output.rownames
      
      return (output)
    }
    
    # final run function
    intermediate.loc.mat <- function(origin, destination, ...){
      locations <- list(...)
      #print("intermediatelocmatlocation")
      #print(locations)
      n <- length(unlist(locations))
      #print("n")
      #print(n)
      search_points <- gcIntermediate(origin, destination, n)
      #print("search points")
      #print(search_points)
      output <- nearest.loc.mat(search_points, locations)
      #print("output")
      #print(output)
      return (output)
    }
    
    # define origin
    origin <- mp_geocode(origin.input,
                         region = 'sg',
                         postcode = NULL,
                         key = key,
                         quiet = F,
                         timeout = 10)
    
    origin <- mp_get_points(origin)
    origin <- c(origin$pnt$`1`[1], origin$pnt$`1`[2])
    
    # define destination
    destination <- mp_geocode(destination.input,
                              region = 'sg',
                              postcode = NULL,
                              key = key,
                              quiet = F,
                              timeout = 10)
    
    destination <- mp_get_points(destination)
    destination <- c(destination$pnt$`1`[1], destination$pnt$`1`[2])
    
    #print("Checkgroup")
    #print(checkGroup)
    #print("destination")
    #print(destination)
    #print("Origin")
    #print(origin)
    
    # define intermediate points
    waypoints <- intermediate.loc.mat(origin, destination, checkGroup.input)
    #print("test")
    #print(test)
    
    # optimise route based on identified points
    if (length(checkGroup.input) == 0){
      doc <- mp_directions(origin = origin,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
      
      print("d if length zero")
      print(d)
      
    } else {
      doc <- mp_directions(origin = origin,
                           waypoints = waypoints,
                           destination = destination,
                           alternatives = F,
                           key = key,
                           mode = "bicycling",
                           quiet = F)
      
      doc <- as_xml_document(doc)
      d <- mp_get_routes(doc)
    }
    
    # plot route on leaflet
    m <- addPolylines(m,
                      data = d,
                      opacity = 1,
                      weight = 4)
    
    print("Arguments before calling addLocationMarkers")
    print(origin)
    print(origin.input)
    print(waypoints)
    print(destination)
    print(destination.input)
    
    # add markers
    addLocationMarkers <- function(map, origin.v, origin.c, waypoints.m, destination.v, destination.c){
      
      iconSet <- awesomeIconList(
        home = makeAwesomeIcon(text = fa("home"), iconColor = "white", markerColor = "blue"),
        dest = makeAwesomeIcon(text = fa("circle"), iconColor = "white", markerColor = "blue"),
        bus_station = makeAwesomeIcon(icon = 'bus', library = 'fa', iconColor = 'yellow', markerColor = 'black'),
        cafe = makeAwesomeIcon(icon = 'coffee', library = 'ion', iconColor = 'white', markerColor = 'brown'),
        meal_takeaway = makeAwesomeIcon(icon = 'cutlery', library = 'glyphicon', iconColor = 'rgb(192, 255, 0)', markerColor = 'darkpurple'),
        convenience_stores = makeAwesomeIcon(icon = 'cart-arrow-down', library = 'fa', iconColor = 'white', markerColor = 'green'),
        park = makeAwesomeIcon(icon = 'tree', library = 'fa', iconColor = 'purple', markerColor = 'yellow'),
        shopping_mall = makeAwesomeIcon(icon = 'shopping-cart', library = 'fa', iconColor = 'white', markerColor = 'pink'),
        tourist_attraction = makeAwesomeIcon(icon = 'industry', library = 'fa', iconColor = 'white', markerColor = 'yellow'),
        gas_stations = makeAwesomeIcon(icon = 'fire', library = 'fa', iconColor = 'black', markerColor = 'orange'),
        train_station = makeAwesomeIcon(icon = 'train', library = 'fa', iconColor = 'white', markerColor = 'darkpurple')
      )
      
      print("Arguments in addLocationMarkers")
      print(origin.v)
      print(origin.c)
      print(waypoints.m)
      print(length(waypoints.m))
      print(destination.v)
      print(destination.c)
      
      if (length(waypoints.m) == 1){
        print("yay")
        m <- rbind(origin.v,
                   destination.v)
        print("m in if add location markers")
        print(m)
        colnames(m) <- c("longitude", "latitude")
        
      } else {
        m <- rbind(rbind(origin.v,
                         waypoints.m),
                   destination.v)
        print("m in else add location markers")
        print(m)
      }
      
      # change the row names of origin and destination
      rownames(m)[1] <- paste0('<strong>', "Start: ", '</strong>',
                               origin.c)
      rownames(m)[nrow(m)] <- paste0('<strong>', "Destination: ", '</strong>',
                                     destination.c)
      
      print("m after conditional exit add location markers")
      print(m)
      
      # map <- map %>%
      #   addAwesomeMarkers(data = m, lng = ~longitude, lat = ~latitude, label = names(m), popup = temp_icon)
      
      m <- asplit(m, 2)
      
      print("m after split add location markers")
      print(m$longitude)
      
      longitudes <- m$longitude
      latitudes <- m$latitude
      
      print(longitudes[1])
      
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
      print(map)
      print("no problem")
      return (map)
    }
    
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
          ##print(i)
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
          ##print(initial_row)
          ##print(max_row)
          i <- i + 1
        }
      }
      
      elevation_df$point_number <- seq.int(nrow(elevation_df)) # creates a column of row numbers in the df
      return(elevation_df)
    }
    
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
    
    # function to get df used to plot main names
    main_df_to_plot <- function(route_df, origin, intermediates, destination){
      main_points <- data.frame(rbind(rbind(origin, intermediates), destination))
      main_points$name <- rownames(main_points)
      point_number_vector <- c()
      route_df.lnglat <- s2_lnglat(route_df$lon, route_df$lat) # convert points to s2 object
      
      for (i in 1:nrow(main_points)){
        main.point.to.search <- s2_lnglat(main_points$lon[[i]], main_points$lat[[i]])
        point_number <- as.integer(s2_closest_feature(main.point.to.search, route_df.lnglat))
        point_number_vector <- append(point_number_vector, point_number)
      }
      
      main_points <- bind_cols(main_points, point_number_vector)
      colnames(main_points) <- c("lon", "lat", "name", "point_number")
      main_points <- left_join(main_points, route_df, by = 'point_number')
      
      return(main_points)
    }
    
    route_points <- route_points(doc)
    route_df <- route_df_to_plot(route_points)
    main_df <- main_df_to_plot(route_df, origin, waypoints, destination)
    main_df[1, 'Total_Distance_metres'] <- 0 # set origin distance to 0
    
    route_df <- left_join(route_df,
                          main_df[, c('point_number', 'name')],
                          by = "point_number")
    route_df[is.na(route_df$name), ]$name <- ''
    
    distance_levels <- route_df %>%
      arrange(by_group = -Total_Distance_metres) %>%
      group_by(L1) %>%
      slice_head()
    
    distance_levels <- distance_levels[, c("L1","Total_Distance_metres")]
    colnames(distance_levels) <- c("L1", "Distance_Levels")
    
    route_df <- left_join(route_df,
                          distance_levels,
                          by = "L1")
    route_df[1, ]$Distance_Levels <- 0
    
    plot_list <- list(waypoints, # for summary
                      addLocationMarkers(m, origin, origin.input, waypoints, destination, destination.input), # for main leaflet plot
                      route_df # for elevation plot
    )
    
    return (plot_list)
  }
  )
  
  url <- "https://weather.com/weather/today/l/1.29,103.85?par=google"
  page <- read_html(url)
  default_site <- "https://weather.com"
  hourly_href <- page %>% html_nodes(".styles--active--3X9QA+ .Button--default--3zkvy") %>% html_attr("href")
  hourly_url <- paste0(default_site, hourly_href)
  
  # reading hourly page
  hourly_page <- read_html(hourly_url)
  
  # Timing (exclude date)
  hourly_data_hr <- hourly_page %>% html_nodes(".DetailsSummary--daypartName--2FBp2") %>% html_text()
  
  # temperature
  hourly_data_temp <- hourly_page %>% html_nodes(".DetailsSummary--tempValue--1K4ka") %>% html_text()
  
  # hourly chance of rain
  hourly_data_rain <- hourly_page %>% html_nodes(".DetailsSummary--precip--1ecIJ") %>% html_text()
  
  # Weather forecast
  hourly_data_w <- hourly_page %>% html_nodes(".DetailsSummary--extendedData--365A_") %>% html_text()
  hourly_data_w <- hourly_data_w[seq(1, length(hourly_data_w), by = 2)]
  
  # Block data
  hourly_data_s <- hourly_page %>% html_nodes(".DetailsTable--DetailsTable--142jU") %>% html_text2()
  
  # Splitting data by "\n"
  col_data <- strsplit(hourly_data_s, split = "\\n")
  
  # colnames
  col_names <- col_data[[1]][seq(1, length(unlist(col_data[1])), by =2)]
  
  # col data
  col_data2 <- lapply(col_data, function(x) x[seq(0, length(unlist(col_data[1])), by = 2)])
  
  df <- as.data.frame(t(as.data.frame(col_data2)))
  colnames(df) <- col_names
  rownames(df) <- NULL
  
  # formatting data
  for (i in 1:ncol(df)){
    if (colnames(df)[i] != "UV Level"){
      
      df[ , i] <- readr::parse_number(df[ , i])
      
    } else {
      df[ , i] <- str_split_fixed(df[ , i], "UV Index", n = 2)
      df[ , i] <- df[ , i][, 2]
    }
  }
  
  df_1 <- data.frame(Hours = hourly_data_hr,
                     Temperature = as.integer((readr::parse_number(hourly_data_temp) - 32) * (5/9)),
                     `Chance of Rain` = readr::parse_number(hourly_data_rain),
                     Status = hourly_data_w)
  
  # merging both files
  df <- cbind(df_1, df)
  
  # date
  date_data <- hourly_page %>% html_nodes(".HourlyForecast--longDate--1tdaJ") %>% html_text()
  
  count <- 1
  
  for (i in 1:nrow(df)){
    
    if((df[i, "Hours"] == "12 am" & i > 1)){
      count <- count + 1
    }
    
    # Add date
    df[i, "Date"] <- date_data[count]
  }
  
  # convert to degree celsius
  df$`Feels-Like Temperature` <- as.integer((df$`Feels-Like Temperature` - 32) * (5/9))
  
  df <- df %>% separate(col = "UV Level",
                        into = c("UV.score", "dummy", "max"),
                        sep = " ",
                        fill = "right")
  
  final.weather.df <- df[1:10,]
  final.weather.df$dummy <- ""
  final.weather.df$Hours <- factor(final.weather.df$Hours, levels = final.weather.df[["Hours"]])
  
  output$map <- renderLeaflet(
    {
      # print("start of render leaflet")
      # print(input$origin)
      # print(input$destination)
      if (input$origin == "" | input$destination == ""){
        m <- leaflet() %>% setView(lat = 1.356660, lng = 103.835381,
                                   zoom = 12) %>% 
          addTiles() %>%
          addMarkers(lat = 1.356660, lng = 103.835381, label = "Singapore")
        
      } else {
        main.map <- trigger.map.and.elevation()
        m <- main.map[[2]]
      }
      m
    }
  )
  
  output$plot <- renderPlotly(
    {
      accumulate_by <- function(dat, var) {
        var <- lazyeval::f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        #print(lvls)
        dats <- lapply(seq_along(lvls),
                       function(x) { cbind(dat[var %in% lvls[seq(1, x)], ],
                                           frame = lvls[[x]])
                       }
        )
        dplyr::bind_rows(dats)
      }
      
      route_df <- trigger.map.and.elevation()[[3]]
      ##print(colnames(route_df))
      test_df <- route_df %>%
        accumulate_by(~Distance_Levels)
      
      fig <- plot_ly(data = test_df,
                     x = ~Total_Distance_metres, 
                     y = ~elevation, 
                     frame = ~frame,
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = 'rgba(67,67,67,1)'),
                     fill = 'tozeroy',
                     fillcolor = "FF8000",
                     text = ~name)
      
      fig <- fig %>%
        layout(yaxis = list(
                 title = "Elevation (metres)", 
                 zeroline = F,
                 tickprefix = ""
               ),
               
               xaxis = list(title = "Distance (metres)", 
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
  )
  
  output$Instruction <- renderUI({       
    fluidRow(
      summaryBox("Insturctions:", 
                 value = p(p("1) Hit 'play' to see your route's elevation animation!"),
                           p("2) Use the slider to manually view the route's elevation"),
                           p("3) Hover over the graph to view exact distances and elevations points within your route!")),
                 width = 12,
                 style = "info"),
    )})
  
  output$summarybox <- renderUI(
    {
      data <- trigger.map.and.elevation()
      way.points <- data[[1]]
      route_df <- data[[3]]
      
      distance <- route_df[nrow(route_df), "Total_Distance_metres"]
      
      print("distance")
      print(distance)
      #distance <- d$distance_m[1]
      duration <- (input$NumHours*60 + input$NumMins)*60
      weight <- input$weight
      speed <- distance/duration
      mph <- speed * 2.236936
      
      MET <- dplyr::case_when(mph < 10 ~ 4,
                              mph < 12 ~ 6.8,
                              mph < 14 ~ 8,
                              mph < 16 ~ 10,
                              mph >= 16 ~ 12)
      ##print(MET)
      
      calories <- MET * as.numeric(weight) * as.numeric(duration/3600)
      weightloss <- calories/7700
      
      amenities <- strsplit(rownames(way.points), split = "<br/>")
      
      amenity.c <- ''
      
      for (amenity in amenities){
        amenity.c <- p(amenity.c,
                       p(amenity[[2]]))
      }
      
      fluidRow(
        summaryBox("Starting Point", input$origin, width = 3, icon = "fas fa-home", style = "info"),
        summaryBox("Destination of Trip", input$destination, width = 3, icon = "fas fa-torii-gate", style = "danger"),
        summaryBox("Distance of Trip", ifelse(distance>1000, paste(distance/1000, 'km'), paste(distance, 'meters')), width = 3, icon = "fas fa-biking", style = "primary"),
        summaryBox("Duration of Trip", paste(input$NumHours, 'Hours', input$NumMins, 'Minutes'), width = 3, icon = "fas fa-clock", style = "success"),
        summaryBox("Recommended Speed for Your Trip", paste(round(speed, 2), 'meters/sec'), width = 3, icon = "fas fa-clock", style = "info"),
        summaryBox("Estimated Calories Burnt", paste(round(calories,0), "cals"), width = 3, icon = "fas fa-fire", style = "danger"), 
        summaryBox("Weight lost", paste(round(weightloss, 3), "kilograms"), width = 3, icon = "fas fa-calendar", style = "primary"),
        summaryBox("Amenities passed by", amenity.c, width = 3, icon = "far fa-building", style = "info")
      )
    }
  )
  
  output$humid.rain <- renderPlot(
    {
      humid.rain <- ggplot(final.weather.df) +
        geom_area(aes(x = Hours, y = Chance.of.Rain), stat = "identity", group = 1, alpha = 0.5, fill = "#69b3a2") +
        geom_line(aes(x = Hours, y = Chance.of.Rain, linetype = 'Chance of Rain'), stat = "identity", group=1) +
        geom_point(aes(x = Hours, y = Chance.of.Rain), stat = "identity", group = 1, size = 2) +
        geom_area(aes(x = Hours, y = Humidity), stat = "identity", group = 1, alpha = 0.3, fill = "paleturquoise1") +
        geom_line(aes(x = Hours, y = Humidity, linetype = 'Humidity'), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = Humidity), stat = "identity", group = 1, size = 2) +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3,
                   colour = "grey") +
        geom_text(aes(x = Hours, y = Chance.of.Rain, label = Chance.of.Rain), position = position_dodge(0.9), vjust = -1) +
        geom_text(aes(x = Hours, y = Humidity, label = Humidity), position = position_dodge(0.9), vjust = -1) +
        ylim(c(0, 100)) +
        theme_classic() +
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom") +
        labs(x = "Hour",
             y = "Humidity and Chance of Rain (%)",
             linetype = "")
      
      return (humid.rain)
    }
  )
  
  output$temperature <- renderPlot(
    {
      status.v <- final.weather.df[, "Status"]
      
      temperature <- ggplot(final.weather.df) +
        geom_line(aes(x = Hours, y = Temperature, linetype = "Temperature"), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = Temperature), stat = "identity", group = 1) +
        geom_line(aes(x = Hours, y = `Feels-Like Temperature`, linetype = 'Feels-like Temperature'), stat = "identity", group = 1) +
        geom_point(aes(x = Hours, y = `Feels-Like Temperature`), stat = "identity", group = 1) +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3, colour = "grey") +
        geom_text(aes(x = Hours, y = Temperature, label = Temperature), position = position_dodge(0.9), vjust = -1) +
        geom_text(aes(x = Hours, y = Temperature, label = Temperature), position = position_dodge(0.9), vjust = -1) +
        geom_text(aes(x = Hours, y = `Feels-Like Temperature`, label = `Feels-Like Temperature`), position = position_dodge(0.9), vjust = -1) +
        theme_classic() +
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              legend.position = "bottom") +
        ylim(min(c(final.weather.df$Temperature, final.weather.df$`Feels-Like Temperature`)) - 1,
             max(c(final.weather.df$Temperature, final.weather.df$`Feels-Like Temperature`)) + 1.5) +
        ylab("Temperature (degree Celsius)") +  
        labs(linetype = '')
      
      for (i in 1:length(status.v)){
        weather <- as.character(status.v[i])
        weather.icon <- readPNG(paste0(getwd(),
                                       "/Images/",
                                       weather,
                                       ".png"),
                                native = T)
        factor = i/10
        left_interval = 0.115
        right_interval = 0.02
        temperature <- temperature +
          inset_element(p = weather.icon,
                        left = ifelse((i == 1),
                                      factor - 0.098,
                                      factor - left_interval),
                        bottom = 0.92,
                        right = factor + right_interval,
                        top = 1)
      }
      return (temperature)
      })
  
  output$uv <- renderPlot(
    {
      uv <- ggplot(final.weather.df) +
        geom_point(aes(x = Hours, y = dummy, size = UV.score), color = "violetred") +
        geom_text(aes(x = Hours, y = dummy, label = UV.score), position = position_dodge(0.9), vjust = 5) +
        theme_classic() +
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              plot.margin = unit(c(3.5,1,3.5,1), 'cm'),
              legend.position = "none") +
        geom_vline(xintercept = final.weather.df$Hours,
                   linetype = "longdash",
                   alpha = 0.3,
                   colour = "grey") +
        ylab("UV Score \n (out of 10)")
        
      return (uv)
      })
}

shinyApp(ui = ui, server = server)