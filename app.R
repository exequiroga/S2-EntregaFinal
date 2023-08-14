## load needed packages for interactive shiny map application
library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(rgdal)
library(DT)
library(readxl)

#install.packages("sf")  # For handling spatial data
library(sf)

#install.packages("rgdal")
library(rgdal)

#install.packages("maptools")
library(maptools)

#install.packages("rgeos")
library(rgeos)



data <- read_excel("zonas.xlsx")
#map <- readOGR(dsn = "C:/Users/exequ/OneDrive/Documentos/FIUBA/Ciencia de Datos/Trabajo de Aplicacion/EntregaSem2",
 #              layer = "caba_zona" )

# read in shape data into global environment 

shapefile <- sf::st_read("caba_zona.shp")

#centers <- cbind.data.frame(data.frame(gCentroid(map, byid=TRUE), id=map@data$zone_id))

region_data <- function(shapefile, markers) {
  
  removeNotification(id = "region_error", session = getDefaultReactiveDomain())
  
  dat <- data.frame(Longitude = markers$lon,
                    Latitude = markers$lat,
                    names = c("Point"))
  
  dat <- sf::st_as_sf(dat,
                      coords = c("Longitude",
                                 "Latitude"))
  
  sf::st_crs(dat) <- sf::st_crs(shapefile)
  
  return(as.data.frame(shapefile)[which(sapply(sf::st_intersects(shapefile,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 6, 
        p("Click or drag the marker wihtin the city of Buenos Aires to see the marker coordinates (lat/long) and what zone it belongs to"),
        leafletOutput(outputId = "map")
    ),
    box(width = 6,
        htmlOutput("text")
    ),
    DTOutput(outputId = "table")
  ),
  title = "Interactive Maps"
)

server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet(shapefile) %>% 
      setView(lng=-58.381592, lat=-34.603722, zoom=11) %>% # center the map in CABA, AR
      addPolygons(color = "black",
                  #fillColor = ~fill_color,
                  fillOpacity = 0.01) %>% 
      addMarkers(lng = -58.381592,
                 lat = -34.603722,
                 options = markerOptions(draggable = TRUE)) %>% 
      #addProviderTiles('Esri.WorldImagery') %>% 
      addTiles()
    
    
  })
  
  current_markers <- reactiveValues(
    lat=-34.603722, lon=-58.381592)
  
  observeEvent(input$map_marker_dragend, {
    
    rd <- region_data(shapefile = shapefile,
                      markers = data.frame(lat = input$map_marker_dragend$lat, lon = input$map_marker_dragend$lng))
    
    if(nrow(rd) == 0){
      showNotification("Error: no data for this location - moving point to previous location!", id = "region_error")
    } else {
      current_markers$lat <- input$map_marker_dragend$lat
      current_markers$lon <- input$map_marker_dragend$lng
    }
    
    # update map after check that the mark is within the defined area
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = current_markers$lat, lng = current_markers$lon),
                 options = markerOptions(draggable = TRUE))
    
  })
  
  observeEvent(input$map_shape_click, {
    
    # update marker location on click
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))
    
    current_markers$lat <- input$map_shape_click$lat
    current_markers$lon <- input$map_shape_click$lng
  }) 
  
  output$text <- renderText({
    paste0("Current marker latitide: ", current_markers$lat, " <br> ",
           "Current marker longitude: ", current_markers$lon, " <br> ",
           if_else(!is.na(region_data(shapefile = shapefile, markers = current_markers)$region), "The marker is in an agricultural region of California.", "The marker is NOT in an agricultural region of California."))
  })
  output$table <- renderDT(data)
}




shinyApp(ui, server)