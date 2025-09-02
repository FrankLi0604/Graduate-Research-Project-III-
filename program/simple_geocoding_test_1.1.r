# 增强版地理编码测试脚本 - 多服务验证版本
# 一次性运行所有代码

# 安装和加载必要的包
{
  required_packages <- c("tidygeocoder", "dplyr", "readr", "stringr", "httr", "jsonlite", "progress", "checkmate", "rlang", "readxl", "writexl", "here", "geosphere")
  
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
    library(geosphere)
  })
  
  cat("All packages loaded successfully!\n")
}

# 配置参数
LAT_MIN <- -29.0; LAT_MAX <- -24.0; LON_MIN <- 150.0; LON_MAX <- 154.0
RATE_LIMIT_SECONDS <- 1.1
COORDINATE_TOLERANCE <- 0.01  # 坐标一致性容差（约1km）
MIN_CONSENSUS <- 2  # 最少需要几个服务返回相似结果才认为可信

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
  
  # 使用OSM Nominatim进行地理编码
  geocode_osm <- function(query_address) {
    tryCatch({
      Sys.sleep(RATE_LIMIT_SECONDS)
      result <- geo(address = query_address, method = "osm", limit = 1, timeout = 30, 
                    custom_query = list(countrycodes = "au", addressdetails = 1))
      
      if (!is.na(result$lat) && !is.na(result$long)) {
        lat <- as.numeric(result$lat); lon <- as.numeric(result$long)
        if (validate_coordinates(lat, lon)) {
          return(list(lat = lat, lon = lon, service = "OSM", success = TRUE))
        }
      }
      return(list(lat = NA, lon = NA, service = "OSM", success = FALSE))
    }, error = function(e) {
      return(list(lat = NA, lon = NA, service = "OSM", success = FALSE))
    })
  }
  
  # 使用ArcGIS进行地理编码
  geocode_arcgis <- function(query_address) {
    tryCatch({
      Sys.sleep(RATE_LIMIT_SECONDS)
      result <- geo(address = query_address, method = "arcgis", limit = 1, timeout = 30)
      
      if (!is.na(result$lat) && !is.na(result$long)) {
        lat <- as.numeric(result$lat); lon <- as.numeric(result$long)
        if (validate_coordinates(lat, lon)) {
          return(list(lat = lat, lon = lon, service = "ArcGIS", success = TRUE))
        }
      }
      return(list(lat = NA, lon = NA, service = "ArcGIS", success = FALSE))
    }, error = function(e) {
      return(list(lat = NA, lon = NA, service = "ArcGIS", success = FALSE))
    })
  }
  
  # 使用Census进行地理编码
  geocode_census <- function(query_address) {
    tryCatch({
      Sys.sleep(RATE_LIMIT_SECONDS)
      result <- geo(address = query_address, method = "census", limit = 1, timeout = 30)
      
      if (!is.na(result$lat) && !is.na(result$long)) {
        lat <- as.numeric(result$lat); lon <- as.numeric(result$long)
        if (validate_coordinates(lat, lon)) {
          return(list(lat = lat, lon = lon, service = "Census", success = TRUE))
        }
      }
      return(list(lat = NA, lon = NA, service = "Census", success = FALSE))
    }, error = function(e) {
      return(list(lat = NA, lon = NA, service = "Census", success = FALSE))
    })
  }
  
  # 直接调用Nominatim API获取更多控制
  geocode_nominatim_direct <- function(query_address) {
    tryCatch({
      Sys.sleep(RATE_LIMIT_SECONDS)
      base_url <- "https://nominatim.openstreetmap.org/search"
      params <- list(
        q = query_address,
        format = "json",
        limit = 1,
        countrycodes = "au",
        addressdetails = 1
      )
      
      response <- GET(base_url, query = params, timeout(30))
      if (response$status_code == 200) {
        content <- content(response, as = "text", encoding = "UTF-8")
        result <- fromJSON(content)
        
        if (length(result) > 0) {
          lat <- as.numeric(result$lat[1])
          lon <- as.numeric(result$lon[1])
          if (validate_coordinates(lat, lon)) {
            return(list(lat = lat, lon = lon, service = "Nominatim_Direct", success = TRUE))
          }
        }
      }
      return(list(lat = NA, lon = NA, service = "Nominatim_Direct", success = FALSE))
    }, error = function(e) {
      return(list(lat = NA, lon = NA, service = "Nominatim_Direct", success = FALSE))
    })
  }
  
  # 计算坐标间的距离
  calculate_distance <- function(lat1, lon1, lat2, lon2) {
    if (any(is.na(c(lat1, lon1, lat2, lon2)))) return(Inf)
    tryCatch({
      return(distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000)  # 返回公里
    }, error = function(e) {
      return(Inf)
    })
  }
  
  # 找到坐标聚类
  find_coordinate_consensus <- function(results) {
    valid_results <- results[!is.na(results$lat) & !is.na(results$lon), ]
    
    if (nrow(valid_results) == 0) {
      return(list(lat = NA, lon = NA, consensus_count = 0, services = "", confidence = "Failed"))
    }
    
    if (nrow(valid_results) == 1) {
      return(list(
        lat = valid_results$lat[1], 
        lon = valid_results$lon[1], 
        consensus_count = 1, 
        services = valid_results$service[1],
        confidence = "Low - Single Source"
      ))
    }
    
    # 寻找相近的坐标点
    clusters <- list()
    for (i in 1:nrow(valid_results)) {
      current_lat <- valid_results$lat[i]
      current_lon <- valid_results$lon[i]
      current_service <- valid_results$service[i]
      
      # 查看是否属于现有聚类
      assigned <- FALSE
      for (j in seq_along(clusters)) {
        cluster_lat <- mean(clusters[[j]]$lat)
        cluster_lon <- mean(clusters[[j]]$lon)
        
        distance <- calculate_distance(current_lat, current_lon, cluster_lat, cluster_lon)
        if (distance <= COORDINATE_TOLERANCE * 111) {  # 转换为公里
          clusters[[j]]$lat <- c(clusters[[j]]$lat, current_lat)
          clusters[[j]]$lon <- c(clusters[[j]]$lon, current_lon)
          clusters[[j]]$services <- c(clusters[[j]]$services, current_service)
          assigned <- TRUE
          break
        }
      }
      
      # 如果不属于任何聚类，创建新聚类
      if (!assigned) {
        clusters[[length(clusters) + 1]] <- list(
          lat = current_lat,
          lon = current_lon,
          services = current_service
        )
      }
    }
    
    # 找到最大的聚类
    cluster_sizes <- sapply(clusters, function(x) length(x$lat))
    best_cluster_idx <- which.max(cluster_sizes)
    best_cluster <- clusters[[best_cluster_idx]]
    
    consensus_count <- length(best_cluster$lat)
    avg_lat <- mean(best_cluster$lat)
    avg_lon <- mean(best_cluster$lon)
    services_used <- paste(best_cluster$services, collapse = ", ")
    
    # 确定置信度
    if (consensus_count >= MIN_CONSENSUS) {
      confidence <- paste("High - Multiple Sources (", consensus_count, " services agree)", sep = "")
    } else {
      confidence <- "Medium - Limited Consensus"
    }
    
    return(list(
      lat = avg_lat, 
      lon = avg_lon, 
      consensus_count = consensus_count,
      services = services_used,
      confidence = confidence
    ))
  }
  
  # 多服务地理编码
  geocode_multiple_services <- function(query_address, strategy_name) {
    cat(paste("  Testing:", substr(query_address, 1, 50), "...\n"))
    
    # 尝试所有免费服务
    services <- list(
      geocode_osm,
      geocode_arcgis,
      geocode_census,
      geocode_nominatim_direct
    )
    
    results <- data.frame(
      lat = numeric(0),
      lon = numeric(0),
      service = character(0),
      stringsAsFactors = FALSE
    )
    
    for (service_func in services) {
      result <- service_func(query_address)
      results <- rbind(results, data.frame(
        lat = result$lat,
        lon = result$lon,
        service = result$service,
        stringsAsFactors = FALSE
      ))
    }
    
    # 寻找一致性结果
    consensus <- find_coordinate_consensus(results)
    
    return(list(
      lat = consensus$lat,
      lon = consensus$lon,
      confidence = consensus$confidence,
      method = paste(strategy_name, " - ", consensus$services, sep = ""),
      consensus_count = consensus$consensus_count,
      success = !is.na(consensus$lat)
    ))
  }
  
  # 带回退的多服务地理编码
  geocode_with_multi_service_fallback <- function(address, suburb, post_code) {
    address <- clean_address_component(address)
    suburb <- clean_address_component(suburb)
    post_code <- as.character(post_code)
    
    cat(paste("Processing:", paste(address, suburb, post_code, sep = ", "), "\n"))
    
    # Strategy 1: 完整地址
    if (address != "" && suburb != "" && post_code != "") {
      query <- paste(address, suburb, post_code, "Australia", sep = ", ")
      result <- geocode_multiple_services(query, "Full address")
      if (result$success && result$consensus_count >= MIN_CONSENSUS) return(result)
    }
    
    # Strategy 2: 地址 + 邮编
    if (address != "" && post_code != "") {
      query <- paste(address, post_code, "Australia", sep = ", ")
      result <- geocode_multiple_services(query, "Address + Post Code")
      if (result$success && result$consensus_count >= MIN_CONSENSUS) return(result)
    }
    
    # Strategy 3: 街道中心
    if (address != "" && suburb != "") {
      street_name <- extract_street_name(address)
      if (street_name != "") {
        query <- paste(street_name, suburb, "Australia", sep = ", ")
        result <- geocode_multiple_services(query, "Street centroid")
        if (result$success && result$consensus_count >= MIN_CONSENSUS) return(result)
      }
    }
    
    # Strategy 4: 区域 + 邮编中心
    if (suburb != "" && post_code != "") {
      query <- paste(suburb, post_code, "Australia", sep = ", ")
      result <- geocode_multiple_services(query, "Suburb centroid")
      if (result$success) return(result)
    }
    
    # Strategy 5: 仅邮编中心
    if (post_code != "") {
      query <- paste(post_code, "Australia", sep = ", ")
      result <- geocode_multiple_services(query, "Post code centroid")
      if (result$success) return(result)
    }
    
    # 如果所有免费服务都失败，标记需要商业API
    return(list(
      lat = NA, 
      lon = NA, 
      confidence = "Failed - May require commercial API (Google Maps, Bing Maps)", 
      method = "All free services failed", 
      consensus_count = 0,
      success = FALSE
    ))
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
run_enhanced_geocoding <- function() {
  cat("=== Enhanced Multi-Service Address Geocoding ===\n")
  cat("This version uses multiple free geocoding services for better accuracy\n\n")
  
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
  cat(paste("\nStarting enhanced geocoding for", nrow(data), "addresses...\n"))
  cat("Using multiple services: OSM, ArcGIS, Census, Nominatim Direct\n")
  cat("This will take longer but provide better accuracy. Progress will be shown.\n\n")
  
  start_time <- Sys.time()
  
  # 创建进度条
  pb <- progress_bar$new(
    format = "[:bar] :percent (:current/:total) ETA: :eta Elapsed: :elapsed",
    total = nrow(data), clear = FALSE, width = 70
  )
  
  # 处理每一行
  results <- vector("list", nrow(data))
  for (i in 1:nrow(data)) {
    pb$tick()
    result <- geocode_with_multi_service_fallback(data$Address[i], data$Suburb[i], data$`Post Code`[i])
    results[[i]] <- list(
      row_id = i, 
      latitude = result$lat, 
      longitude = result$lon,
      confidence = result$confidence, 
      geocoding_method = result$method,
      service_consensus = result$consensus_count
    )
  }
  
  # 合并结果
  results_df <- bind_rows(results) %>%
    left_join(data %>% mutate(row_id = row_number()), by = "row_id") %>%
    select(-row_id) %>%
    select(everything(), latitude, longitude, confidence, geocoding_method, service_consensus)
  
  # 保存结果
  write_xlsx(results_df, output_file)
  
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  
  # 生成详细摘要
  successful <- results_df %>% filter(!is.na(latitude))
  high_confidence <- results_df %>% filter(str_detect(confidence, "High"))
  failed <- results_df %>% filter(is.na(latitude))
  need_commercial <- results_df %>% filter(str_detect(confidence, "commercial API"))
  
  cat("\n=== Enhanced Geocoding Complete ===\n")
  cat(paste("Total addresses processed:", nrow(results_df), "\n"))
  cat(paste("Successfully geocoded:", nrow(successful), paste0("(", round(100 * nrow(successful) / nrow(results_df), 1), "%)\n")))
  cat(paste("High confidence results:", nrow(high_confidence), paste0("(", round(100 * nrow(high_confidence) / nrow(results_df), 1), "%)\n")))
  cat(paste("Failed addresses:", nrow(failed), paste0("(", round(100 * nrow(failed) / nrow(results_df), 1), "%)\n")))
  cat(paste("May need commercial API:", nrow(need_commercial), paste0("(", round(100 * nrow(need_commercial) / nrow(results_df), 1), "%)\n")))
  cat(paste("Processing time:", round(processing_time, 2), "minutes\n"))
  cat(paste("Results saved to:", output_file, "\n\n"))
  
  if (nrow(need_commercial) > 0) {
    cat("NOTE: Some addresses could not be geocoded with free services.\n")
    cat("Consider using Google Maps Geocoding API or Bing Maps API for these addresses.\n")
    cat("They are marked in the 'confidence' column.\n\n")
  }
  
  return(results_df)
}

# 现在你可以运行：
cat("Enhanced setup complete! Run the following command to start geocoding:\n")
cat("results <- run_enhanced_geocoding()\n\n")
cat("Features of enhanced version:\n")
cat("- Uses 4 different free geocoding services\n")
cat("- Compares results across services for accuracy\n")
cat("- Marks high-confidence results (multiple services agree)\n")
cat("- Identifies addresses that may need commercial APIs\n")
cat("- Provides detailed consensus information\n")