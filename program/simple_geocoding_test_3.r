# 简化版地理编码测试脚本
# 一次性运行所有代码

# 安装和加载必要的包
{
  required_packages <- c("tidygeocoder", "dplyr", "readr", "stringr", "httr", "jsonlite", "progress", "checkmate", "rlang", "readxl", "writexl", "here")
  
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages)
  }
  
  suppressPackageStartupMessages({
    library(tidygeocoder)
    library(dplyr)
    library(readr)
    library(stringr)
    library(httr)
    library(jsonlite)
    library(progress)
    library(checkmate)
    library(rlang)
    library(readxl)
    library(writexl)
    library(here)
  })
  
  cat("All packages loaded successfully!\n")
}

# 配置参数
LAT_MIN <- -29.0; LAT_MAX <- -24.0; LON_MIN <- 150.0; LON_MAX <- 154.0; RATE_LIMIT_SECONDS <- 1.1

# 所有函数定义
{
  clean_address_component <- function(address_component) {
    if (is.na(address_component) || is.null(address_component)) return("")
    cleaned <- str_trim(str_squish(as.character(address_component)))
    cleaned <- str_to_title(cleaned)
    cleaned <- str_replace_all(cleaned, "\\bSt\\b", "Street")
    cleaned <- str_replace_all(cleaned, "\\bRd\\b", "Road")
    cleaned <- str_replace_all(cleaned, "\\bAve\\b", "Avenue")
    cleaned <- str_replace_all(cleaned, "\\bDr\\b", "Drive")
    cleaned <- str_replace_all(cleaned, "\\bCt\\b", "Court")
    cleaned <- str_replace_all(cleaned, "\\bPl\\b", "Place")
    return(cleaned)
  }
  
  extract_street_name <- function(address) {
    if (is.na(address) || address == "") return("")
    street <- str_replace(address, "^\\d+[a-zA-Z]?\\s+", "")
    street <- str_replace(street, "^\\d+/\\d+\\s+", "")
    street <- str_replace(street, "^\\d+-\\d+\\s+", "")
    return(str_trim(street))
  }
  
  validate_coordinates <- function(lat, lon) {
    if (is.na(lat) || is.na(lon)) return(FALSE)
    return(lat >= LAT_MIN && lat <= LAT_MAX && lon >= LON_MIN && lon <= LON_MAX)
  }
  
  determine_confidence <- function(method) {
    if (str_detect(method, "Full address")) return("High")
    else if (str_detect(method, "Address \\+ Post Code|Street centroid")) return("Medium")
    else return("Low")
  }
  
  geocode_single_address <- function(query_address, method) {
    Sys.sleep(RATE_LIMIT_SECONDS)
    result <- geo(address = query_address, method = "osm", limit = 1, timeout = 30, 
                  custom_query = list(countrycodes = "au", addressdetails = 1, extratags = 1))
    
    if (!is.na(result$lat) && !is.na(result$long)) {
      lat <- as.numeric(result$lat); lon <- as.numeric(result$long)
      if (validate_coordinates(lat, lon)) {
        return(list(lat = lat, lon = lon, confidence = determine_confidence(method), method = method, success = TRUE))
      }
    }
    return(list(lat = NA, lon = NA, confidence = "Failed", method = "No match found", success = FALSE))
  }
  
  geocode_with_fallback <- function(address, suburb, post_code) {
    address <- clean_address_component(address)
    suburb <- clean_address_component(suburb)
    post_code <- as.character(post_code)
    
    # Strategy 1: Full address
    if (address != "" && suburb != "" && post_code != "") {
      query <- paste(address, suburb, post_code, "Australia", sep = ", ")
      result <- geocode_single_address(query, "Full address match")
      if (result$success) return(result)
    }
    
    # Strategy 2: Address + Post Code
    if (address != "" && post_code != "") {
      query <- paste(address, post_code, "Australia", sep = ", ")
      result <- geocode_single_address(query, "Address + Post Code")
      if (result$success) return(result)
    }
    
    # Strategy 3: Street centroid
    if (address != "" && suburb != "") {
      street_name <- extract_street_name(address)
      if (street_name != "") {
        query <- paste(street_name, suburb, "Australia", sep = ", ")
        result <- geocode_single_address(query, "Street centroid")
        if (result$success) return(result)
      }
    }
    
    # Strategy 4: Suburb + Post Code centroid
    if (suburb != "" && post_code != "") {
      query <- paste(suburb, post_code, "Australia", sep = ", ")
      result <- geocode_single_address(query, "Suburb centroid")
      if (result$success) return(result)
    }
    
    # Strategy 5: Post Code centroid
    if (post_code != "") {
      query <- paste(post_code, "Australia", sep = ", ")
      result <- geocode_single_address(query, "Post code centroid")
      if (result$success) return(result)
    }
    
    return(list(lat = NA, lon = NA, confidence = "Failed", method = "No match found", success = FALSE))
  }
  
  check_system_requirements <- function() {
    tryCatch({
      response <- GET("https://nominatim.openstreetmap.org/", timeout(10))
      if (response$status_code != 200) stop("Cannot connect to Nominatim geocoding service")
    }, error = function(e) stop("Cannot connect to Nominatim geocoding service - check internet connection"))
    
    tryCatch({
      test_result <- geo("Brisbane, Queensland, Australia", method = "osm", limit = 1)
      if (is.na(test_result$lat)) stop("Nominatim geocoding service is not responding correctly")
    }, error = function(e) stop("Error testing geocoding service - service may be unavailable"))
    
    return(TRUE)
  }
  
  cat("All functions defined successfully!\n")
}

# 主要运行函数
run_simple_geocoding <- function() {
  cat("=== Simple Address Geocoding Test ===\n")
  
  # 检查系统要求
  cat("Checking system requirements...\n")
  check_system_requirements()
  cat("System requirements met!\n\n")
  
  # 获取输入文件路径
  cat("Please enter the full path to your Excel file:\n")
  input_file <- readline("File path: ")
  
  # 移除引号
  input_file <- str_replace_all(input_file, "[\"\']", "")
  
  # 检查文件是否存在
  if (!file.exists(input_file)) {
    stop(paste("File not found:", input_file))
  }
  
  # 读取Excel文件
  cat("Reading Excel file...\n")
  data <- read_excel(input_file)
  
  # 检查必需的列
  required_cols <- c("Address", "Suburb", "Post Code")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # 显示数据预览
  cat("\nData preview (first 3 rows):\n")
  print(head(data, 3))
  
  # 获取输出文件路径
  cat("\nPlease enter the output Excel file path:\n")
  output_file <- readline("Output path: ")
  output_file <- str_replace_all(output_file, "[\"\']", "")
  
  # 确保输出文件有.xlsx扩展名
  if (!str_detect(tolower(output_file), "\\.xlsx$")) {
    output_file <- paste0(tools::file_path_sans_ext(output_file), ".xlsx")
  }
  
  # 开始地理编码
  cat(paste("\nStarting geocoding for", nrow(data), "addresses...\n"))
  cat("This may take a while. Progress will be shown.\n\n")
  
  start_time <- Sys.time()
  
  # 创建进度条
  pb <- progress_bar$new(
    format = "[:bar] :percent (:current/:total) ETA: :eta",
    total = nrow(data), clear = FALSE, width = 60
  )
  
  # 处理每一行
  results <- vector("list", nrow(data))
  for (i in 1:nrow(data)) {
    pb$tick()
    result <- geocode_with_fallback(data$Address[i], data$Suburb[i], data$`Post Code`[i])
    results[[i]] <- list(
      row_id = i, latitude = result$lat, longitude = result$lon,
      confidence = result$confidence, geocoding_method = result$method
    )
  }
  
  # 合并结果
  results_df <- bind_rows(results) %>%
    left_join(data %>% mutate(row_id = row_number()), by = "row_id") %>%
    select(-row_id) %>%
    select(everything(), latitude, longitude, confidence, geocoding_method)
  
  # 保存结果
  write_xlsx(results_df, output_file)
  
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  
  # 显示摘要
  successful <- results_df %>% filter(!is.na(latitude))
  cat("\n=== Geocoding Complete ===\n")
  cat(paste("Total addresses:", nrow(results_df), "\n"))
  cat(paste("Successful:", nrow(successful), paste0("(", round(100 * nrow(successful) / nrow(results_df), 1), "%)\n")))
  cat(paste("Processing time:", round(processing_time, 2), "minutes\n"))
  cat(paste("Results saved to:", output_file, "\n\n"))
  
  return(results_df)
}

# 现在你可以运行：
cat("Setup complete! Run the following command to start geocoding:\n")
cat("results <- run_simple_geocoding()\n")