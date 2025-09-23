library(shiny)
library(leaflet)
library(readxl)
library(DT)
library(htmlwidgets)

save_available <- FALSE

ui <- fluidPage(
  titlePanel("Queensland Case Mapping Visualization System"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("File Operations"),
      
      fileInput("file", "Select Excel File",
                accept = c(".xlsx", ".xls")),
      
      hr(),
      
      h4("Map Settings"),
      selectInput("map_style", "Map Style",
                  choices = list(
                    "OpenStreetMap" = "OpenStreetMap",
                    "Satellite Map" = "Esri.WorldImagery", 
                    "Simple Map" = "CartoDB.Positron"
                  ),
                  selected = "OpenStreetMap"),
      
      sliderInput("marker_size", "Marker Size",
                  min = 3, max = 15, value = 6, step = 1),
      
      hr(),
      
      h4("Data Statistics"),
      verbatimTextOutput("data_summary")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "600px"),
      
      br(),
      
      h4("Data Preview"),
      DT::dataTableOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    tryCatch({
      df <- readxl::read_excel(input$file$datapath)
      
      required_cols <- c("Job Number", "Date Recieved", "Reported Animal Breed", "Family", "Genus", "Species", "latitude", "longitude")
      missing_cols <- required_cols[!required_cols %in% names(df)]
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")), 
                        type = "error")
        return(NULL)
      }
      
      df_clean <- df[!is.na(df$latitude) & !is.na(df$longitude) & 
                     df$latitude != "" & df$longitude != "", ]
      
      df_clean$latitude <- as.numeric(df_clean$latitude)
      df_clean$longitude <- as.numeric(df_clean$longitude)
      
      df_clean <- df_clean[!is.na(df_clean$latitude) & !is.na(df_clean$longitude), ]
      
      df_clean <- df_clean[df_clean$latitude >= -30 & df_clean$latitude <= -9 &
                          df_clean$longitude >= 137 & df_clean$longitude <= 155, ]
      
      return(df_clean)
    }, error = function(e) {
      showNotification(paste("File reading error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$map <- renderLeaflet({
    df <- data()
    
    map <- leaflet() %>%
      setView(lng = 145.7781, lat = -20.9176, zoom = 6)
    
    map_style <- input$map_style
    if (is.null(map_style)) {
      map_style <- "OpenStreetMap"
    }
    
    if (map_style == "OpenStreetMap") {
      map <- map %>% addTiles()
    } else if (map_style == "Esri.WorldImagery") {
      map <- map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else if (map_style == "CartoDB.Positron") {
      map <- map %>% addProviderTiles(providers$CartoDB.Positron)
    } else {
      map <- map %>% addTiles()
    }
    
    if (!is.null(df) && nrow(df) > 0) {
      marker_size <- input$marker_size
      if (is.null(marker_size)) {
        marker_size <- 6
      }
      
      popup_content <- paste0(
        "<b>Job Number:</b> ", df$`Job Number`, "<br>",
        "<b>Date Received:</b> ", df$`Date Recieved`, "<br>",
        "<b>Animal Breed:</b> ", df$`Reported Animal Breed`, "<br>",
        "<b>Family:</b> ", ifelse(is.na(df$Family) | df$Family == "", "Not specified", df$Family), "<br>",
        "<b>Genus:</b> ", ifelse(is.na(df$Genus) | df$Genus == "", "Not specified", df$Genus), "<br>",
        "<b>Species:</b> ", ifelse(is.na(df$Species) | df$Species == "", "Not specified", df$Species), "<br>",
        "<b>Latitude:</b> ", df$latitude, "<br>",
        "<b>Longitude:</b> ", df$longitude
      )
      
      map <- map %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          radius = marker_size,
          color = "blue",
          fillColor = "blue",
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = popup_content
        )
    }
    
    return(map)
  })
  
  output$data_summary <- renderText({
    df <- data()
    if (is.null(df)) {
      return("Please select an Excel file first")
    }
    
    total_cases <- nrow(df)
    
    family_count <- sum(!is.na(df$Family) & df$Family != "", na.rm = TRUE)
    genus_count <- sum(!is.na(df$Genus) & df$Genus != "", na.rm = TRUE)
    species_count <- sum(!is.na(df$Species) & df$Species != "", na.rm = TRUE)
    
    paste0(
      "Valid cases: ", total_cases, "\n",
      "Cases with Family info: ", family_count, "\n",
      "Cases with Genus info: ", genus_count, "\n", 
      "Cases with Species info: ", species_count, "\n",
      "Latitude range: ", round(min(df$latitude), 4), " to ", round(max(df$latitude), 4), "\n",
      "Longitude range: ", round(min(df$longitude), 4), " to ", round(max(df$longitude), 4)
    )
  })
  
  output$data_table <- DT::renderDataTable({
    df <- data()
    if (is.null(df)) return(NULL)
    
    DT::datatable(df, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)