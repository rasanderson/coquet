library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(DT)

# GeoJSON URL
geojson_url <- "https://services-eu1.arcgis.com/MSNNjkZ51iVh8yBj/arcgis/rest/services/Northumbrian_Water_Storm_Overflow_Activity_2_view/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# Define UI
ui <- fluidPage(
  titlePanel("Northumbrian Water Storm Overflow Activity"),
  DTOutput(outputId = "table"),
  downloadButton("download_daily_csv", "Download today's flow CSV"),
  downloadButton("download_all_csv",   "Download all flow CSV"),
  
  leafletOutput("map", height = "800px")
)

# Define Server
server <- function(input, output, session) {
  # Load GeoJSON data
  geo_data <- reactive({
    req <- GET(geojson_url)
    geojson <- content(req, as = "text", encoding = "UTF-8")
    data <- st_read(geojson)
    
    # Convert timestamps
    data$StatusStart <- as.POSIXct(data$StatusStart / 1000, origin = "1970-01-01", tz = "UTC")
    data$LatestEventStart <- as.POSIXct(data$LatestEventStart / 1000, origin = "1970-01-01", tz = "UTC")
    data$LatestEventEnd <- as.POSIXct(data$LatestEventEnd / 1000, origin = "1970-01-01", tz = "UTC")
    data$LastUpdated <- as.POSIXct(data$LastUpdated / 1000, origin = "1970-01-01", tz = "UTC")
    
    return(data)
  })
  
  output$table <- renderDT({
    my_tbl <- data.frame(geo_data())
    datatable(my_tbl)
  })
  
  
  output$download_daily_csv <- downloadHandler(
    filename = function() {
      paste0("storm_overflow_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      # Columns 11 and 12 duplicate geometry
      write.csv(data.frame(geo_data())[, -c(11,12)], file, row.names = FALSE)
    }
  )
  
  
  output$download_all_csv <- downloadHandler(
    filename = function() {
      paste0("storm_overflow_data_all.csv")
    },
    content = function(file) {
      all_data <- read.csv("cumulative_flow.csv", row.names = NULL)
      write.csv(all_data, file, row.names = FALSE)
    }
  )  
  
  # # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(geo_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 5,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<strong>Id:</strong> ", Id, "<br>",
          "<strong>Company:</strong> ", Company, "<br>",
          "<strong>Status:</strong> ", Status, "<br>",
          "<strong>Status Start:</strong> ", StatusStart, "<br>",
          "<strong>Latest Event Start:</strong> ", LatestEventStart, "<br>",
          "<strong>Latest Event End:</strong> ", LatestEventEnd, "<br>",
          "<strong>Receiving Water Course:</strong> ", ReceivingWaterCourse, "<br>",
          "<strong>Last Updated:</strong> ", LastUpdated
        )
      )
  })
}

# Run the app
shinyApp(ui, server)
