library(tidyverse) # A bunch of useful functions/libraries that makes R more awesome
library(shiny) # R's Web Framework
library(shinydashboard) # A cool theme for Shiny
library(leaflet) # Open source package for interactive maps
library(plotly) # Open source package for interactive plots
options(stringsAsFactors = F) # Make sure R doesn't read in strings as factor types


#### Global Playground ####
## In this space, we'll load in and do some initial transformations on our data

fires = read.csv("Columbus_Fire_Calls.csv")
# Transform needed columns from strings into datetime types
fires$EventFirstDispatched <- as.POSIXct(fires$EventFirstDispatched, format = "%m/%d/%Y %H:%M")
fires$EventFirstArrive <- as.POSIXct(fires$EventFirstArrive, format = "%m/%d/%Y %H:%M")
# Only grab rows where we know the exact location of the fire
fires = fires %>% filter(!is.na(LastKnownLatitude) & !is.na(LastKnownLongitude))
# Create a new column, telling us how long it took the fire department to get to the scene
fires$ResponseTime = difftime(fires$EventFirstArrive, fires$EventFirstDispatched, units = "mins")


#### Shiny App ####
shinyApp(
  
### UI ###
  ui = dashboardPage(
    # A header, sidebar, and body are all required elements of a shinydashboard
    dashboardHeader(title = "Cool Fire Dashboard App!"),
    # The sidebar is what you'll see on the right side of the app
    dashboardSidebar(
      # Allow the user to choose which Engine they want to look at
      selectInput("engine", "Search Engine:", choices = unique(fires$FirstArriveEngine)),
      # Seperate the app into two 'pages'
      sidebarMenu(
        menuItem("Plots", tabName = "plots", icon=icon("line-chart")),
        menuItem("Mapping", tabName="map", icon=icon("map"))
      ),
      # Allow the user to choose which incident call type(s) to focus on
      selectInput("call_type", "Call Type:", choices = unique(fires$Call_Type), multiple = TRUE),
      # Allow the user to select a range of incident call dates
      dateRangeInput("call_date", "Call Date:", start = min(fires$EventFirstDispatched, na.rm = T), end = max(fires$EventFirstDispatched, na.rm = T))
    ),
    # All visualizations/mapping will go in the body, or the main part, of the app
    dashboardBody(
      # Tell R to expect seperate the body UI elements out into our defined tabs
      tabItems(
        tabItem("plots",
                # Output function for plotly visualizations
                plotlyOutput("first_plot")
        ),
        tabItem("map",
                # Output function for leaflet maps
                leafletOutput("fire_map", width="100%", height="1000px")
        )
      )
    )
    
  ),
  
### SERVER ###
  server = function(input, output) {
    # Create a 'reactive' dataframe that will reinitialize every time an input is changed
    selected_fires <- reactive({
      fires %>%
        filter(
          Call_Type %in% input$call_type,
          FirstArriveEngine == input$engine,
          EventFirstDispatched > input$call_date[1],
          EventFirstDispatched < input$call_date[2]
        )
    })
    
    # Render our first plotly visualization
    output$first_plot = renderPlotly({
      # Calculate the average call response time per month in 2018
      response_trend = selected_fires() %>% 
        group_by(Create_Month) %>% 
        summarize(AvgResponseTime = median(ResponseTime))
      
      p = response_trend %>%
        ggplot(aes(x = Create_Month, y = AvgResponseTime)) +
        # Use stat = "identity" when you want to specify y values in a bar plot!
        geom_bar(stat = "identity")
      # Use the ggplot -> plotly conversion function
      p %>% ggplotly
    })
    
    # Render our leaflet map
    output$fire_map = renderLeaflet({
      leaflet(selected_fires()) %>%
        # Add generic map 'tiles' to the leaflet
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        # Tell leaflet where to 'aim'
        setView(lng = -82.99, lat = 39.961, zoom = 11) %>%
        # Add markers for specific fire incidents
        addMarkers(lng = ~LastKnownLongitude, lat = ~LastKnownLatitude, popup = ~paste(Call_Source, Call_Type))
        
      })
  }
)