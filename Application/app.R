#Initialize packages
library(shiny); library(leaflet); library(ggmap);library(geojsonio); library(DT)


#Load School Data
schoolData <- readRDS("Datasets/schoolDataSubset.RDS")
lasteaedData <- readRDS("Datasets/lasteaedData.RDS")

#Load Crash Data
crashData <- readRDS("Datasets/crashDataCleanedFixed.RDS")

#Load Crimedata
crimeProperty <- readRDS("Datasets/crimeProperty.RDS")
crimeState <- readRDS("Datasets/crimeStateMerged.RDS")
crimeTraffic <- readRDS("Datasets/crimeTrafficMerged.RDS")

#Get Linnaosa Districts
#linnaosaJSON <- geojson_read("Datasets/geojsonboundaries.json",what = "sp")



#Change from LEST 97 Coordinates
#schoolData <- eestiGPSConvert(schoolData,schoolData$y, schoolData$x)



#User Interface
ui <- navbarPage(
  navbarPage(
    "Tallinn Real Estate Pilot Program",
    tabPanel("Map of Tallinn",
             sidebarLayout(
               sidebarPanel(
                 textInput("address", "Address", value = ""),
                 verbatimTextOutput("value"),
                 actionButton("addressButton", "Search"),
                 p(
                   "Type in an address and press 'search' to mark the location on the map!"
                 )
               ),
               mainPanel(
                 tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
                 leafletOutput("outputmap", width = "100%", height = "800px"),
                 hr(
                   "This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 693849."
                 )
                 
               )
             )),
    tabPanel(
      title = "Crime Data Exploration",

   fluidRow(
     column(4,
            selectInput("linnaosa",
                        "Linnaosa:",
                        c("All",
                          unique(as.character(crimeProperty$KohtNimetus))))
     ),
     column(4,
            selectInput("type",
                        "Crime Type:",
                        c("All",
                          unique(as.character(crimeProperty$ParagrahvTais))))
     ),
     column(4,
            selectInput("hind",
                        "Total Damage:",
                        c("All",
                          unique(as.character(crimeProperty$Kahjusumma))))
     )
   ),
  # Create a new row for the table.
   fluidRow(
     div(DT::dataTableOutput("table"))
   )),
    tabPanel(title = "About", 
                    fluidRow(
                      column(8, offset = 1,
                             includeMarkdown("about.md"))))
           

))

#Server
server <- function(input, output) {
  #Create Data Table
    output$table <- DT::renderDataTable(DT::datatable({
    data <- crimeProperty
    input$test

    data <- crimeProperty
    if (input$linnaosa != "All") {
      data <- data[data$KohtNimetus == input$linnaosa,]
    }
    if (input$type != "All") {
      data <- data[data$ParagrahvTais == input$type,]
    }
    if (input$hind != "All") {
      data <- data[data$Kahjusumma == input$hind,]
    }
    data
  }))

  ##Create Custom Icons Here
  schoolIcons <- awesomeIcons(
    icon = 'graduation-cap',
    markerColor = 'lightblue',
    library = 'fa',
    iconColor = 'black'
  )
  
  lasteaedIcons <- awesomeIcons(
    icon = 'graduation-cap',
    markerColor = 'darkpurple',
    library = 'fa',
    iconColor = 'black'
  )
  
  addressIcons <- awesomeIcons(
    icon = 'home',
    markerColor = "red",
    library = 'fa',
    iconColor = 'black'
  )
  
  function(input, output, session) {
    
    ##Initializes the leaflet map for the page.
    output$outputmap <- renderLeaflet({
      map <-
        leaflet() %>% addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
        setView(lng =  24.753574,
                lat = 59.436962,
                zoom = 12) %>%
        addAwesomeMarkers (
          lng = schoolData$Lon,
          lat = schoolData$Lat,
          icon = schoolIcons,
          popup = paste(
            "Name:",
            schoolData$Nimi,
            "<br>",
            "Type:",
            schoolData$Type,
            "<br>",
            "Teaching Language:",
            lasteaedData$Õppekeel,
            "<br>",
            "Address:",
            schoolData$Aadress
          ),
          group = "Schools"
        ) %>%
        addAwesomeMarkers (
          lng = lasteaedData$Lon,
          lat = lasteaedData$Lat,
          icon = lasteaedIcons,
          popup = paste(
            "Name:",
            lasteaedData$Nimi,
            "<br>",
            "Type:",
            lasteaedData$Type,
            "<br>",
            "Teaching Language:",
            lasteaedData$Õppekeel,
            "<br>",
            "Address:",
            lasteaedData$Aadress
          ),
          group = "Kindergartens"
        ) %>%
        
        addAwesomeMarkers(
          lng = crashData$Lon,
          lat = crashData$Lat,
          clusterOptions = markerClusterOptions(),
          popup = paste(
            "Date:",
            crashData$Kuupäev,
            "<br>",
            "Information:",
            crashData$Situatsiooni.tüüp,
            "<br>",
            "Time:",
            crashData$Kellaaeg,
            "<br>",
            "Damage (EUR):",
            crashData$Kahju.suurus..euro.
          ),
          group = "Car Accidents"
        ) %>%
        
        addLayersControl(
          overlayGroups = c("Schools","Kindergartens", "Car Accidents", "Crime", "Bus Stops"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Schools")    %>%
        hideGroup("Kindergartens")    %>%
        hideGroup("Car Accidents")     %>%
        hideGroup("Crime")     %>%
        hideGroup("Bus Stops")
    })
    
    
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
        icon = addressIcons,
        label = input$address
      )
    })
    
  }
  }  
  
# Run Application 
shinyApp(ui = ui, server = server)
