library(shinydashboard)
library(googleVis)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinyTime)
library(hms)


route_coord <- readr::read_csv("./shapes1.csv")
stops_coord <- readr::read_csv("./stops.csv")
routes <- readr::read_csv("./routes.csv")
lirr_sched_df2 <- readr::read_csv("./lirr_sched_df2.csv") 

ui <- fluidPage(shinyUI(
  dashboardPage(
    dashboardHeader(title = 'LIRR Delays'),
    dashboardSidebar(
      sidebarUserPanel(name = 'Joseph C. Fritch'),
      #sidebarMenu(
        #menuItem("Map", tabName = "map", icon = icon("map")),
        #menuItem("Data", tabName = "data", icon = icon("database"))
      #),
      # Select Departure station
      selectizeInput(inputId = "station_depart", label = "Select Departure Station", stops_coord$stop_name),
      # Select Arrival station
      selectizeInput(inputId = "station_arrive", label = "Select Arrival Station", stops_coord$stop_name ),
      # Input date train selection
      dateInput("date", "Select Departure Date:", value = "2019-01-21"),
      # Input departure time for train selection
      #timeInput("time", "Select Departure Time:", value = Sys.time(),)
      div( id = "eg2",
           fluidRow(
             column(width = 11, offset = 1,
                    h4('Select Departure Time'),
                    sliderInput("slider_hours", "Hours:", 
                                min=0, max=23, value=0, step = 1),
                    sliderInput("slider_mins", "Mins:", 
                                min = 0, max = 59, value = 0, step = 1),
                    h4(textOutput("output_slider"))
             )
           ),
           shiny::hr()
      ),
      # Select Trip
      selectizeInput(inputId = "train", label = "Select Train", 
                     as.POSIXct(lirr_sched_df2$departure_time,format="%H:%M:%S")),

      #Action Button
      actionButton("button", "Show Results")
    ),
    
    dashboardBody(
        leafletOutput("mymap")
      )
  )
))

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = route_coord)%>%
    #leaflet(data = filter(route_coord, shape_name == route) %>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      addPolylines(~shape_pt_lon, ~ shape_pt_lat)
      #addCircleMarkers(~stop_lon, ~stop_lat)
  })
}
shinyApp(ui, server)