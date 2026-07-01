library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(DT)

# GeoJSON URLs
# Storm overflow activity data from Northumbrian Water
geojson_soa_url <- "https://services-eu1.arcgis.com/MSNNjkZ51iVh8yBj/arcgis/rest/services/Northumbrian_Water_Storm_Overflow_Activity_2_view/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# Define UI
ui <- fluidPage(
  titlePanel("Northumbrian Water Storm Overflow Activity"),
  # downloadButton("download_daily_csv", "Download today's flow CSV"),
  downloadButton("download_all_csv", "Download all flow CSV"),
  checkboxInput("show_soa", "Show storm overflow points", value = FALSE),
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; gap: 10px; align-items: center; margin: 8px 0;",
        tags$strong("Hierarchy:"),
        textOutput("breadcrumb", inline = TRUE),
        actionButton("back_level", "Back"),
        actionButton("reset_level", "Reset to Northumbria")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      div(style = "overflow-x: auto;", DTOutput(outputId = "table"))
    ),
    column(
      width = 6,
      leafletOutput("map", height = "800px")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  hierarchy_base_url <- "https://environment.data.gov.uk/catchment-planning"
  type_to_path <- c(
    RiverBasinDistrict = "RiverBasinDistrict",
    ManagementCatchment = "ManagementCatchment",
    OperationalCatchment = "OperationalCatchment",
    WaterBody = "WaterBody"
  )
  child_type <- c(
    RiverBasinDistrict = "ManagementCatchment",
    ManagementCatchment = "OperationalCatchment",
    OperationalCatchment = "WaterBody",
    WaterBody = NA
  )

  hierarchy_state <- reactiveValues(
    stack = data.frame(
      type = "RiverBasinDistrict",
      id = "3",
      name = "Northumbria",
      stringsAsFactors = FALSE
    )
  )

  feature_cache <- new.env(parent = emptyenv())

  get_page_url <- function(type, id) {
    paste0(hierarchy_base_url, "/", type_to_path[[type]], "/", id)
  }

  get_geojson_url <- function(type, id) {
    paste0(get_page_url(type, id), ".geojson")
  }

  fetch_feature <- function(type, id) {
    cache_key <- paste(type, id, sep = "::")
    if (exists(cache_key, envir = feature_cache, inherits = FALSE)) {
      return(get(cache_key, envir = feature_cache, inherits = FALSE))
    }

    req <- GET(get_geojson_url(type, id))
    geojson <- content(req, as = "text", encoding = "UTF-8")
    data <- suppressWarnings(st_read(geojson, quiet = TRUE))

    if (!"id" %in% names(data)) {
      data$id <- id
    }
    if (!"name" %in% names(data)) {
      data$name <- id
    }

    assign(cache_key, data, envir = feature_cache)
    data
  }

  extract_children <- function(parent_type, parent_id) {
    next_type <- child_type[[parent_type]]
    if (is.na(next_type)) {
      return(data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE))
    }

    req <- GET(get_page_url(parent_type, parent_id))
    html <- content(req, as = "text", encoding = "UTF-8")
    path_name <- type_to_path[[next_type]]
    pattern <- paste0(
      "<a[^>]+href=[\"'][^\"']*/catchment-planning/",
      path_name,
      "/([A-Za-z0-9]+)[^\"']*[\"'][^>]*>.*?</a>"
    )
    links <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]

    if (length(links) == 0) {
      return(data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE))
    }

    ids <- sub(paste0("^.*?/catchment-planning/", path_name, "/([A-Za-z0-9]+).*$"), "\\1", links, perl = TRUE)
    labels <- sub("^.*?>", "", links, perl = TRUE)
    labels <- sub("</a>.*$", "", labels, perl = TRUE)
    labels <- gsub("<[^>]+>", "", labels, perl = TRUE)
    labels <- gsub("&amp;", "&", labels, fixed = TRUE)
    labels <- gsub("&nbsp;", " ", labels, fixed = TRUE)
    labels <- trimws(gsub("\\s+", " ", labels))
    labels[labels == ""] <- ids[labels == ""]

    child_df <- data.frame(id = ids, name = labels, stringsAsFactors = FALSE)
    child_df[!duplicated(child_df$id), , drop = FALSE]
  }

  as_polygons <- function(sf_obj) {
    geom_types <- as.character(st_geometry_type(sf_obj, by_geometry = TRUE))
    polygons <- sf_obj[geom_types %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
    polygons[!st_is_empty(polygons), , drop = FALSE]
  }

  as_lines <- function(sf_obj) {
    geom_types <- as.character(st_geometry_type(sf_obj, by_geometry = TRUE))
    lines <- sf_obj[geom_types %in% c("LINESTRING", "MULTILINESTRING"), , drop = FALSE]
    lines[!st_is_empty(lines), , drop = FALSE]
  }

  get_valid_bbox <- function(sf_obj) {
    if (is.null(sf_obj) || nrow(sf_obj) == 0) {
      return(NULL)
    }

    bbox <- suppressWarnings(st_bbox(sf_obj))
    bbox_vals <- as.numeric(bbox)
    if (length(bbox_vals) != 4 || any(!is.finite(bbox_vals))) {
      return(NULL)
    }

    if (bbox["xmin"] >= bbox["xmax"] || bbox["ymin"] >= bbox["ymax"]) {
      return(NULL)
    }

    list(
      xmin = as.numeric(bbox["xmin"]),
      ymin = as.numeric(bbox["ymin"]),
      xmax = as.numeric(bbox["xmax"]),
      ymax = as.numeric(bbox["ymax"])
    )
  }

  summarize_child_feature <- function(child_type, child_id, child_name = child_id) {
    raw <- fetch_feature(child_type, child_id)

    child_polygons <- as_polygons(raw)
    if (nrow(child_polygons) > 0) {
      child_polygons <- st_sf(
        data.frame(id = child_id, name = child_name, stringsAsFactors = FALSE),
        geometry = st_sfc(st_union(st_geometry(child_polygons)), crs = st_crs(child_polygons))
      )
    }

    child_lines <- as_lines(raw)
    if (nrow(child_lines) > 0) {
      child_lines <- st_sf(
        data.frame(id = child_id, name = child_name, stringsAsFactors = FALSE),
        geometry = st_sfc(st_union(st_geometry(child_lines)), crs = st_crs(child_lines))
      )
    }

    list(polygons = child_polygons, lines = child_lines)
  }

  current_node <- reactive({
    hierarchy_state$stack[nrow(hierarchy_state$stack), , drop = FALSE]
  })

  hierarchy_layer <- reactive({
    node <- current_node()
    current_type <- node$type[[1]]
    current_id <- node$id[[1]]
    next_type <- child_type[[current_type]]

    if (is.na(next_type)) {
      leaf <- fetch_feature("WaterBody", current_id)
      return(list(
        layer_type = "WaterBody",
        polygons = as_polygons(leaf),
        lines = as_lines(leaf),
        next_type = NA
      ))
    }

    child_info <- extract_children(current_type, current_id)
    if (nrow(child_info) == 0) {
      return(list(layer_type = next_type, polygons = NULL, lines = NULL, next_type = next_type))
    }

    summarized <- lapply(seq_len(nrow(child_info)), function(i) {
      summarize_child_feature(next_type, child_info$id[[i]], child_info$name[[i]])
    })
    polygon_list <- Filter(function(x) !is.null(x$polygons) && nrow(x$polygons) > 0, summarized)
    line_list <- Filter(function(x) !is.null(x$lines) && nrow(x$lines) > 0, summarized)

    combined_polygons <- if (length(polygon_list) > 0) {
      do.call(rbind, lapply(polygon_list, function(x) x$polygons))
    } else {
      NULL
    }

    combined_lines <- if (length(line_list) > 0) {
      do.call(rbind, lapply(line_list, function(x) x$lines))
    } else {
      NULL
    }

    list(
      layer_type = next_type,
      polygons = combined_polygons,
      lines = combined_lines,
      next_type = next_type
    )
  })

  output$breadcrumb <- renderText({
    paste(hierarchy_state$stack$name, collapse = " > ")
  })

  observeEvent(input$back_level, {
    if (nrow(hierarchy_state$stack) > 1) {
      hierarchy_state$stack <- hierarchy_state$stack[-nrow(hierarchy_state$stack), , drop = FALSE]
    }
  })

  observeEvent(input$reset_level, {
    hierarchy_state$stack <- hierarchy_state$stack[1, , drop = FALSE]
  })

  # Load GeoJSON data
  soa_data <- reactive({
    req <- GET(geojson_soa_url)
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
    my_tbl <- data.frame(soa_data())
    hidden_cols <- c("OBJECTID", "Company", "geometry")
    keep_cols <- setdiff(names(my_tbl), hidden_cols)
    datatable(my_tbl[, keep_cols, drop = FALSE])
  })


  output$download_daily_csv <- downloadHandler(
    filename = function() {
      paste0("storm_overflow_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      # Columns 11 and 12 duplicate geometry
      write.csv(data.frame(soa_data())[, -c(11, 12)], file, row.names = FALSE)
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

  observeEvent(input$map_shape_click, {
    layer <- hierarchy_layer()
    req(!is.null(layer$polygons))

    clicked_id <- input$map_shape_click$id
    req(!is.null(clicked_id), nzchar(clicked_id))

    clicked <- layer$polygons[layer$polygons$id == clicked_id, , drop = FALSE]
    req(nrow(clicked) > 0)

    next_type <- layer$next_type
    req(!is.na(next_type))

    hierarchy_state$stack <- rbind(
      hierarchy_state$stack,
      data.frame(
        type = next_type,
        id = as.character(clicked$id[[1]]),
        name = as.character(clicked$name[[1]]),
        stringsAsFactors = FALSE
      )
    )
  })

  # Render Leaflet map
  output$map <- renderLeaflet({
    layer <- hierarchy_layer()

    map <- leaflet() %>% addTiles()

    if (isTRUE(input$show_soa)) {
      map <- map %>%
        addCircleMarkers(
          data = soa_data(),
          radius = 5,
          color = "blue",
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = ~ paste0(
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
    }

    if (!is.null(layer$polygons) && nrow(layer$polygons) > 0) {
      map <- map %>%
        addPolygons(
          data = layer$polygons,
          layerId = ~id,
          color = "#b22222",
          weight = 2,
          fillColor = "#b22222",
          fillOpacity = 0.08,
          opacity = 0.9,
          popup = ~name
        )
    }

    show_river_network <- identical(layer$layer_type, "WaterBody")

    if (show_river_network && !is.null(layer$lines) && nrow(layer$lines) > 0) {
      map <- map %>%
        addPolylines(
          data = layer$lines,
          color = "#1f77b4",
          weight = 2,
          opacity = 0.8,
          popup = ~name
        )
    }

    bbox <- get_valid_bbox(layer$polygons)
    if (is.null(bbox) && show_river_network) {
      bbox <- get_valid_bbox(layer$lines)
    }

    if (!is.null(bbox)) {
      map <- map %>% fitBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    }

    map
  })
}

# Run the app
shinyApp(ui, server)
