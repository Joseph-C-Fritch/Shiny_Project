library(googleVis)
library(leaflet)
library(shiny)
library(maps)

route_coord <- readr::read_csv("./shapes1.csv")
stops_coord <- readr::read_csv("./stops.csv")


ui <- fluidPage(shinyUI(
  dashboardPage(
    dashboardHeader(title = 'LIRR Delays'),
    dashboardSidebar(
      sidebarUserPanel(
        name = 'Joseph C. Fritch',
        subtitle = 'Choose Station'
      ),
      sidebarMenu(
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Data", tabName = "data", icon = icon("database"))
      ),
      selectizeInput("selected",
                     "Select Branch To Display",
                     choice)
    ),
    
    dashboardBody(
      
      
      tabItems(
          
        tabItem(
          tabName = "Map",
          box(
            title = "LIRR Map",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("mymap")
          )
        ),
        
        
        ## ui.R ##
        tabItem(tabName = "map",
                fluidRow(box(htmlOutput("hist"), height = 300))),
        
        
        tabItem(tabName = "data",
                # datatable
                fluidRow(box(DT::dataTableOutput("table"))))
        
        
      ))
  )
))



server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = route_coord) %>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      addPolylines(~shape_pt_lon, ~shape_pt_lat)
      #addCircleMarkers(~stop_lon, ~stop_lat)
    
  })
  

  observeEvent(input$show, {
    proxy <- leafletProxy("mymap")
    if(input$show) {
      proxy %>% addPolygons(data=colStates, stroke = FALSE,
                            fillColor = heat.colors(6, alpha = 1),
                            
                            layerId = LETTERS[1:6])
    } else {
      proxy %>% removeShape(layerId = LETTERS[1:6])
    }
  })
}
shinyApp(ui, server)