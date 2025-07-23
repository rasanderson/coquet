# Generate all the CRAG data as CSV but keep appending

library(jsonlite)
library(httr)
library(sf)

# GeoJSON URL
geojson_url <- "https://services-eu1.arcgis.com/MSNNjkZ51iVh8yBj/arcgis/rest/services/Northumbrian_Water_Storm_Overflow_Activity_2_view/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

geo_data <- function(geojson_url){
  req <- GET(geojson_url)
  geojson <- content(req, as = "text", encoding = "UTF-8")
  data <- st_read(geojson)
  
  # Convert time stamps
  data$StatusStart <- as.POSIXct(data$StatusStart / 1000, origin = "1970-01-01", tz = "UTC")
  data$LatestEventStart <- as.POSIXct(data$LatestEventStart / 1000, origin = "1970-01-01", tz = "UTC")
  data$LatestEventEnd <- as.POSIXct(data$LatestEventEnd / 1000, origin = "1970-01-01", tz = "UTC")
  data$LastUpdated <- as.POSIXct(data$LastUpdated / 1000, origin = "1970-01-01", tz = "UTC")
  
  return(data)
}

today_data <- data.frame(geo_data(geojson_url))
today_data <- today_data[, -c(11, 12)] # don't repeat geometry

# Daily data
# Get the current date in yyyymmdd format
date_str <- format(Sys.Date(), "%Y%m%d")
# Also add query data as a column to output table
today_data$query_date <- format(Sys.Date(), "%Y-%m-%d")
# Create the filename
daily_file <- paste0("daily_data_", date_str, ".csv")
write.csv(today_data, file = daily_file, row.names = FALSE)

if(!file.exists("cumulative_flow.csv")){
  write.table(today_data, "cumulative_flow.csv", append = TRUE, row.names = FALSE,
              sep = ",", col.names = TRUE)
} else {
write.table(today_data, "cumulative_flow.csv", append = TRUE, row.names = FALSE,
            sep = ",", col.names = FALSE)
}
