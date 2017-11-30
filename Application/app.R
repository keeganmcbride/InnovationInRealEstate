#Initialize packages
library(shiny); library(leaflet); library(ggmap);


#Load School Data
schoolData <- readRDS("datasets/schoolDataSubset.RDS")
JulasteaedData <- readRDS("datasets/lasteaedData.RDS")

#Load Crash Data
crashData <- readRDS("datasets/crashDataCleanedFixed.RDS")

#Load Crimedata
crimeProperty <- readRDS("datasets/crimeProperty.RDS")
crimeState <- readRDS("datasets/crimeStateMerged.RDS")
crimeTraffic <- readRDS("datasets/crimeTrafficMerged.RDS")

#Get Linnaosa Districts



#Change from LEST 97 Coordinates
schoolData <- eestiGPSConvert(schoolData,schoolData$y, schoolData$x)



#User Interface
ui <- navbarPage(
  tabPanel(title = "Map of Tallinn",
           sidebarLayout(
             sidebarPanel(
               textInput("address", "Address", value = ""),
               verbatimTextOutput("value"),
               actionButton("addressButton", "Search"),
               p(
                 "Type in your address, press search 'Search', and see your location on the map!"
               )
             ),
             mainPanel(
               tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
               leafletOutput("outputmap", width = "100%", height = "800px"),
               hr(
                 "This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 693849."
               )
           )
           
)))





#Server
server <- function(input, output) {
  output$outputmap <- renderLeaflet({
    map <-
      leaflet() %>% addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
      setView(lng =  24.753574,
              lat = 59.436962,
              zoom = 12) })
  ##Geocoding of custom address input
  geocoding <-
    eventReactive(input$addressButton, {
      geocode(input$address)
    })
  
  #Takes an address input and places a marker on the map at given location
  observeEvent(input$addressButton, {
    newMarker <- geocoding()
    leafletProxy('outputmap') %>% addAwesomeMarkers(
      lng = newMarker$lon,
      lat = newMarker$lat,
      label = input$address
    )
  })
  
}



# Run Application 
shinyApp(ui = ui, server = server)
