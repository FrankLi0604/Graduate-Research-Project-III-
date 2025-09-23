library(shiny)
library(leaflet)
library(readxl)
library(DT)
library(htmlwidgets)
library(lubridate)

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
      
      h4("Color Legend"),
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
        div(
          style = "margin-bottom: 5px;",
          span(style = "display: inline-block; width: 15px; height: 15px; background-color: #FF8C00; border-radius: 50%; margin-right: 8px; vertical-align: middle;"),
          "Daytime Cases (5:00-19:00)"
        ),
        div(
          style = "margin-bottom: 5px;",
          span(style = "display: inline-block; width: 15px; height: 15px; background-color: #0066CC; border-radius: 50%; margin-right: 8px; vertical-align: middle;"),
          "Nighttime Cases (19:01-4:59)"
        ),
        div(
          span(style = "display: inline-block; width: 15px; height: 15px; background-color: #9966CC; border-radius: 50%; margin-right: 8px; vertical-align: middle;"),
          "Unknown Time Cases"
        )
      ),
      
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
      
      # 添加时间分类功能
      df_clean$time_category <- "Unknown"
      df_clean$marker_color <- "#9966CC"  # 紫色 - 未知时间
      
      # 处理日期时间 - 直接处理POSIXct格式
      tryCatch({
        # 确保Date Recieved列存在且不全为NA
        if ("Date Recieved" %in% names(df_clean)) {
          date_col <- df_clean[["Date Recieved"]]
          
          # 检查数据类型
          if (inherits(date_col, "POSIXct") || inherits(date_col, "POSIXt")) {
            # 数据已经是日期时间格式，直接处理
            valid_dates <- !is.na(date_col)
            
            showNotification(paste("Found", sum(valid_dates), "valid POSIXct dates out of", length(date_col), "total records"), 
                           type = "message", duration = 8)
            
            if (sum(valid_dates) > 0) {
              # 直接提取小时
              valid_datetimes <- date_col[valid_dates]
              hours <- as.numeric(format(valid_datetimes, "%H"))
              
              # 分类：5:00-19:00为白天，其余为夜晚
              daytime <- hours >= 5 & hours < 19
              
              # 初始化时间分类列
              df_clean$time_category <- "Unknown"
              df_clean$marker_color <- "#9966CC"  # 紫色 - 未知时间
              
              # 更新有效日期的行
              valid_indices <- which(valid_dates)
              df_clean$time_category[valid_indices[daytime]] <- "Daytime"
              df_clean$time_category[valid_indices[!daytime]] <- "Nighttime"
              df_clean$marker_color[valid_indices[daytime]] <- "#FF8C00"  # 橙色 - 白天
              df_clean$marker_color[valid_indices[!daytime]] <- "#0066CC"  # 蓝色 - 夜晚
              
              # 显示分类统计
              showNotification(paste("Successfully categorized:", sum(daytime), "daytime and", sum(!daytime), "nighttime cases"), 
                             type = "message", duration = 8)
              
            } else {
              showNotification("No valid dates found in POSIXct column", type = "warning")
              df_clean$time_category <- "Unknown"
              df_clean$marker_color <- "#9966CC"
            }
            
          } else {
            # 如果不是POSIXct格式，尝试转换
            showNotification(paste("Date column is", class(date_col)[1], "attempting conversion"), 
                           type = "message", duration = 8)
            
            # 尝试转换为POSIXct
            converted_dates <- tryCatch({
              as.POSIXct(date_col, tz = "UTC")
            }, error = function(e) {
              as.POSIXct(as.character(date_col), tz = "UTC")
            })
            
            valid_dates <- !is.na(converted_dates)
            
            if (sum(valid_dates) > 0) {
              hours <- as.numeric(format(converted_dates[valid_dates], "%H"))
              daytime <- hours >= 5 & hours < 19
              
              df_clean$time_category <- "Unknown"
              df_clean$marker_color <- "#9966CC"
              
              valid_indices <- which(valid_dates)
              df_clean$time_category[valid_indices[daytime]] <- "Daytime"
              df_clean$time_category[valid_indices[!daytime]] <- "Nighttime"
              df_clean$marker_color[valid_indices[daytime]] <- "#FF8C00"
              df_clean$marker_color[valid_indices[!daytime]] <- "#0066CC"
              
              showNotification(paste("Converted and categorized:", sum(daytime), "daytime and", sum(!daytime), "nighttime cases"), 
                             type = "message", duration = 8)
            } else {
              df_clean$time_category <- "Unknown"
              df_clean$marker_color <- "#9966CC"
            }
          }
        } else {
          showNotification("Column 'Date Recieved' not found in the data", type = "warning")
          df_clean$time_category <- "Unknown"
          df_clean$marker_color <- "#9966CC"
        }
        
      }, error = function(e) {
        showNotification(paste("Date processing error:", e$message), type = "warning", duration = 10)
        # 确保即使出错也有默认值
        if (!"time_category" %in% names(df_clean)) {
          df_clean$time_category <- "Unknown"
        }
        if (!"marker_color" %in% names(df_clean)) {
          df_clean$marker_color <- "#9966CC"
        }
      })
      
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
        "<b>Job Number:</b> ", df[["Job Number"]], "<br>",
        "<b>Date Received:</b> ", df[["Date Recieved"]], "<br>",
        "<b>Time Category:</b> ", df$time_category, "<br>",
        "<b>Animal Breed:</b> ", df[["Reported Animal Breed"]], "<br>",
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
          color = df$marker_color,
          fillColor = df$marker_color,
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
    
    # 时间分类统计
    daytime_count <- sum(df$time_category == "Daytime", na.rm = TRUE)
    nighttime_count <- sum(df$time_category == "Nighttime", na.rm = TRUE)
    unknown_count <- sum(df$time_category == "Unknown", na.rm = TRUE)
    
    paste0(
      "Valid cases: ", total_cases, "\n",
      "Daytime cases: ", daytime_count, "\n",
      "Nighttime cases: ", nighttime_count, "\n",
      "Unknown time cases: ", unknown_count, "\n",
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
    
    # 添加时间分类列到数据表显示
    display_df <- df
    display_df <- display_df[, !names(display_df) %in% "marker_color"]  # 移除颜色列，不在表格中显示
    
    DT::datatable(display_df, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)