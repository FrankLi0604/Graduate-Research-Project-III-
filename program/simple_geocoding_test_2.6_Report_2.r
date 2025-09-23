# 最终生产版地理编码脚本 - 基于修复后的功能
# 整合所有修复和改进 - 增强进度显示版本

# 安装和加载必要的包
{
  required_packages <- c("tidygeocoder", "dplyr", "readr", "stringr", "httr", "jsonlite", 
                        "progress", "checkmate", "rlang", "readxl", "writexl", "here", 
                        "tcltk", "parallel", "fs", "lubridate", "R6", "purrr")
  
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
    library(tcltk)
    library(parallel)
    library(fs)
    library(lubridate)
    library(R6)
    library(purrr)
  })
  
  cat("All packages loaded successfully!\n")
}

# 全局配置参数
GLOBAL_CONFIG <- list(
  # 地理范围 - 扩大昆士兰州及周边范围
  LAT_MIN = -29.5,
  LAT_MAX = -8.0,
  LON_MIN = 137.0,
  LON_MAX = 154.0,
  
  # 性能参数
  RATE_LIMIT_SECONDS = 1.1,
  BATCH_SIZE = 500,            # 减小批次大小以提高稳定性
  SAVE_INTERVAL = 50,          # 更频繁保存进度
  PROGRESS_UPDATE_INTERVAL = 100,  # 每100条更新一次进度
  
  # 文件管理
  TEMP_FOLDER_PREFIX = "geocoding_temp_",
  LOG_FILE = "geocoding_log.txt",
  PROGRESS_FILE = "progress.rds",
  
  # 监控参数
  SUCCESS_RATE_THRESHOLD = 0.7,
  MAX_CONSECUTIVE_FAILURES = 25,  # 降低连续失败阈值
  SPEED_CALCULATION_WINDOW = 100   # 最近N条记录用于速度计算
)

# 修复后的地址字段映射
ADDRESS_FIELD_MAPPINGS_FINAL <- list(
  # 标准字段
  address = c("Address", "address", "ADDRESS", "Street Address", "street_address"),
  suburb = c("Suburb", "suburb", "SUBURB", "City", "city", "CITY"),
  postcode = c("Post Code", "Postcode", "postcode", "POSTCODE", "ZIP", "zip", "Post_Code"),
  
  # L/F字段 - 处理多行列名
  lf_complete = c(
    "L/F Address (Complete)", 
    "L/F Address Complete", 
    "LF Address Complete", 
    "LF_Address_Complete", 
    "L/F_Address_Complete",
    "L/F Address\n(Complete)",
    "L/F Address (Complete)"  # 确保精确匹配
  ),
  lf_street = c(
    "L/F Street", 
    "LF Street", 
    "L/F_Street", 
    "LF_Street"
  ),
  lf_cross_street = c(
    "L/F Nearest Cross Street", 
    "LF Nearest Cross Street", 
    "L/F_Nearest_Cross_Street", 
    "LF_Nearest_Cross_Street",
    "L/F Cross Street", 
    "LF Cross Street",
    "L/F Nearest\nCross Street"
  ),
  lf_undefined = c(
    "L/F Undefined", 
    "LF Undefined", 
    "L/F_Undefined", 
    "LF_Undefined"
  ),
  lf_suburb_state_post = c(
    "L/F Suburb / State / Post", 
    "LF Suburb State Post",
    "L/F_Suburb_State_Post", 
    "LF_Suburb_State_Post",
    "L/F Suburb/State/Post",
    "L/F Suburb /\nState / Post"
  ),
  
  # 其他位置字段
  physical_location = c(
    "Physical Location", 
    "physical_location", 
    "PHYSICAL_LOCATION", 
    "Physical_Location", 
    "PhysicalLocation",
    "Physical\nLocation"
  ),
  shelter_location = c(
    "Shelter Location", 
    "shelter_location", 
    "SHELTER_LOCATION",
    "Shelter_Location", 
    "ShelterLocation",
    "Shelter\nLocation"
  )
)

# 核心函数定义
{
  # 改进的字段检测函数
  detect_address_fields_final <- function(column_names) {
    detected_fields <- list()
    
    # 清理列名
    cleaned_column_names <- sapply(column_names, function(name) {
      if (is.na(name) || is.null(name)) return("")
      cleaned <- str_replace_all(as.character(name), "\\s*\n\\s*", " ")
      cleaned <- str_replace_all(cleaned, "\\s+", " ")
      cleaned <- str_trim(cleaned)
      return(cleaned)
    })
    
    for (field_type in names(ADDRESS_FIELD_MAPPINGS_FINAL)) {
      possible_names <- ADDRESS_FIELD_MAPPINGS_FINAL[[field_type]]
      matched_name <- NULL
      
      # 精确匹配
      for (possible_name in possible_names) {
        if (possible_name %in% column_names) {
          matched_name <- possible_name
          break
        }
      }
      
      # 清理后匹配
      if (is.null(matched_name)) {
        for (possible_name in possible_names) {
          for (i in seq_along(cleaned_column_names)) {
            if (cleaned_column_names[i] == possible_name) {
              matched_name <- column_names[i]
              break
            }
          }
          if (!is.null(matched_name)) break
        }
      }
      
      detected_fields[[field_type]] <- matched_name
    }
    
    return(detected_fields)
  }
  
  # 改进的数据清理函数
  clean_address_component_final <- function(address_component) {
    if (is.na(address_component) || is.null(address_component) || 
        address_component == "NA" || address_component == "NULL" || 
        address_component == "" || str_trim(as.character(address_component)) == "") {
      return("")
    }
    
    cleaned <- tryCatch({
      str_trim(str_squish(as.character(address_component)))
    }, error = function(e) {
      return("")
    })
    
    if (is.na(cleaned) || cleaned == "" || cleaned == "NA") return("")
    
    # 检查无意义模式
    meaningless_patterns <- c(
      "^n/a$", "^na$", "^null$", "^unknown$", "^-+$", "^\\.+$", 
      "^\\?+$", "^x+$", "^nil$", "^none$", "^blank$", "^empty$",
      "^a/a$", "^aa$", "^0+$", "^n\\.?a\\.?$"
    )
    
    if (any(str_detect(str_to_lower(cleaned), meaningless_patterns))) {
      return("")
    }
    
    if (nchar(cleaned) < 3 && !str_detect(cleaned, "^\\d{4}$")) {
      return("")
    }
    
    # 地址标准化
    cleaned <- str_to_title(cleaned)
    cleaned <- str_replace_all(cleaned, "\\bSt\\b", "Street")
    cleaned <- str_replace_all(cleaned, "\\bRd\\b", "Road")
    cleaned <- str_replace_all(cleaned, "\\bAve\\b", "Avenue")
    cleaned <- str_replace_all(cleaned, "\\bDr\\b", "Drive")
    cleaned <- str_replace_all(cleaned, "\\bCt\\b", "Court")
    cleaned <- str_replace_all(cleaned, "\\bPl\\b", "Place")
    cleaned <- str_replace_all(cleaned, "\\bQld\\b", "Queensland")
    cleaned <- str_replace_all(cleaned, "\\bQLD\\b", "Queensland")
    
    if (is.na(cleaned)) return("")
    return(cleaned)
  }
  
  safe_not_empty <- function(x) {
    return(!is.na(x) && !is.null(x) && str_trim(as.character(x)) != "")
  }
  
  validate_coordinates <- function(lat, lon) {
    if (is.na(lat) || is.na(lon)) return(FALSE)
    return(lat >= GLOBAL_CONFIG$LAT_MIN && lat <= GLOBAL_CONFIG$LAT_MAX && 
           lon >= GLOBAL_CONFIG$LON_MIN && lon <= GLOBAL_CONFIG$LON_MAX)
  }
  
  geocode_single_address_final <- function(query_address, method) {
    Sys.sleep(GLOBAL_CONFIG$RATE_LIMIT_SECONDS)
    
    tryCatch({
      # 确保在澳大利亚境内搜索
      if (!str_detect(str_to_lower(query_address), "australia|queensland|qld")) {
        query_address <- paste(query_address, "Queensland, Australia")
      }
      
      result <- geo(address = query_address, method = "osm", limit = 1, timeout = 30, 
                    custom_query = list(countrycodes = "au", addressdetails = 1))
      
      if (!is.na(result$lat) && !is.na(result$long)) {
        lat <- as.numeric(result$lat)
        lon <- as.numeric(result$long)
        if (validate_coordinates(lat, lon)) {
          return(list(lat = lat, lon = lon, method = method, success = TRUE))
        }
      }
    }, error = function(e) {
      # 静默处理错误
    })
    
    return(list(lat = NA, lon = NA, method = "No match found", success = FALSE))
  }
  
  # 优化的地理编码策略
  geocode_with_enhanced_fallback_final <- function(detected_fields, row_data) {
    fields_used <- c()
    
    # 提取字段
    lf_complete <- ""
    lf_suburb_state_post <- ""
    lf_street <- ""
    
    if (!is.null(detected_fields$lf_complete)) {
      lf_complete <- clean_address_component_final(row_data[[detected_fields$lf_complete]])
      if (safe_not_empty(lf_complete)) fields_used <- c(fields_used, "lf_complete")
    }
    
    if (!is.null(detected_fields$lf_suburb_state_post)) {
      lf_suburb_state_post <- clean_address_component_final(row_data[[detected_fields$lf_suburb_state_post]])
      if (safe_not_empty(lf_suburb_state_post)) fields_used <- c(fields_used, "lf_suburb_state_post")
    }
    
    if (!is.null(detected_fields$lf_street)) {
      lf_street <- clean_address_component_final(row_data[[detected_fields$lf_street]])
      if (safe_not_empty(lf_street)) fields_used <- c(fields_used, "lf_street")
    }
    
    if (length(fields_used) == 0) {
      return(list(lat = NA, lon = NA, confidence = "Failed", 
                  method = "No valid address fields", success = FALSE, 
                  fields_used = "None"))
    }
    
    # 策略1: L/F完整地址（最高优先级）
    if (safe_not_empty(lf_complete)) {
      result <- geocode_single_address_final(lf_complete, "L/F Complete address")
      if (result$success) {
        result$confidence <- "Very High"
        result$fields_used <- paste(fields_used, collapse = ", ")
        return(result)
      }
    }
    
    # 策略2: 街道+郊区组合
    if (safe_not_empty(lf_street) && safe_not_empty(lf_suburb_state_post)) {
      query <- paste(lf_street, lf_suburb_state_post, sep = ", ")
      result <- geocode_single_address_final(query, "L/F Street + Suburb")
      if (result$success) {
        result$confidence <- "High"
        result$fields_used <- paste(fields_used, collapse = ", ")
        return(result)
      }
    }
    
    # 策略3: 仅郊区信息
    if (safe_not_empty(lf_suburb_state_post)) {
      result <- geocode_single_address_final(lf_suburb_state_post, "L/F Suburb only")
      if (result$success) {
        result$confidence <- "Medium"
        result$fields_used <- paste(fields_used, collapse = ", ")
        return(result)
      }
    }
    
    return(list(lat = NA, lon = NA, confidence = "Failed", 
                method = "All strategies failed", success = FALSE, 
                fields_used = paste(fields_used, collapse = ", ")))
  }
  
  check_system_requirements <- function() {
    tryCatch({
      test_result <- geo("Brisbane, Queensland, Australia", method = "osm", limit = 1)
      if (is.na(test_result$lat)) stop("Geocoding service not responding")
    }, error = function(e) stop("Error testing geocoding service"))
    
    return(TRUE)
  }
  
  reorder_output_columns <- function(results_df, original_data, detected_fields) {
    result_columns <- c("latitude", "longitude", "confidence", "geocoding_method", "fields_used")
    original_columns <- names(original_data)
    
    final_order <- c(original_columns, result_columns)
    final_order <- final_order[final_order %in% names(results_df)]
    
    return(results_df[, final_order])
  }
}

# 文件选择函数（简化版）
select_file_gui <- function(title = "选择Excel文件") {
  cat("\n=== 文件选择 ===\n")
  
  # 尝试RStudio API
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch({
      file_path <- rstudioapi::selectFile(
        caption = title,
        filter = "Excel Files (*.xlsx *.xls)",
        existing = TRUE
      )
      if (!is.null(file_path) && file_path != "") {
        return(file_path)
      }
    }, error = function(e) {})
  }
  
  # 系统对话框
  tryCatch({
    file_path <- file.choose()
    if (!is.null(file_path) && file_path != "") {
      return(file_path)
    }
  }, error = function(e) {})
  
  # 手动输入
  cat("请输入Excel文件的完整路径（使用正斜杠 /）:\n")
  while (TRUE) {
    file_path <- readline("文件路径: ")
    file_path <- str_trim(str_replace_all(file_path, "[\"\']", ""))
    file_path <- str_replace_all(file_path, "\\\\", "/")
    
    if (file.exists(file_path) && str_detect(tolower(file_path), "\\.(xlsx|xls)$")) {
      return(file_path)
    }
    cat("文件不存在或不是Excel格式，请重新输入\n")
  }
}

# 保存位置选择
select_save_location_gui <- function(default_name = "geocoded_results.xlsx") {
  cat("\n=== 选择保存位置 ===\n")
  
  # 尝试RStudio API
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch({
      save_path <- rstudioapi::selectFile(
        caption = "选择保存位置",
        filter = "Excel Files (*.xlsx)",
        existing = FALSE
      )
      if (!is.null(save_path) && save_path != "") {
        if (!str_detect(tolower(save_path), "\\.xlsx$")) {
          save_path <- paste0(tools::file_path_sans_ext(save_path), ".xlsx")
        }
        return(save_path)
      }
    }, error = function(e) {})
  }
  
  # 使用默认位置
  default_path <- file.path(getwd(), default_name)
  cat("使用默认保存位置:", default_path, "\n")
  return(default_path)
}

# 增强版进度监控类
EnhancedProgressMonitor <- R6Class("EnhancedProgressMonitor",
  public = list(
    total_rows = 0,
    processed_rows = 0,
    successful_rows = 0,
    failed_rows = 0,
    start_time = NULL,
    last_update_time = NULL,
    last_success_rate = 1.0,
    consecutive_failures = 0,
    processing_speeds = numeric(),  # 存储最近的处理速度
    memory_usage = numeric(),       # 存储内存使用记录
    last_batch_processed = 0,
    
    initialize = function(total_rows) {
      self$total_rows <- total_rows
      self$start_time <- Sys.time()
      self$last_update_time <- self$start_time
      cat("\n" %R% strrep("=", 80) %R% "\n")
      cat("🚀 开始地理编码处理\n")
      cat(sprintf("总数据量: %s 条记录\n", format(total_rows, big.mark = ",")))
      cat(sprintf("开始时间: %s\n", format(self$start_time, "%Y-%m-%d %H:%M:%S")))
      cat(strrep("=", 80) %R% "\n\n")
    },
    
    update = function(success, batch_size = 1) {
      self$processed_rows <- self$processed_rows + batch_size
      if (success) {
        self$successful_rows <- self$successful_rows + batch_size
        self$consecutive_failures <- 0
      } else {
        self$failed_rows <- self$failed_rows + batch_size
        self$consecutive_failures <- self$consecutive_failures + batch_size
      }
      self$last_success_rate <- self$successful_rows / self$processed_rows
      
      # 计算当前处理速度（基于最近N条记录）
      current_time <- Sys.time()
      if (length(self$processing_speeds) >= GLOBAL_CONFIG$SPEED_CALCULATION_WINDOW) {
        self$processing_speeds <- self$processing_speeds[-1]  # 移除最旧的记录
      }
      
      time_diff <- as.numeric(difftime(current_time, self$last_update_time, units = "secs"))
      if (time_diff > 0) {
        current_speed <- batch_size / time_diff
        self$processing_speeds <- c(self$processing_speeds, current_speed)
      }
      
      self$last_update_time <- current_time
      self$last_batch_processed <- batch_size
      
      # 记录内存使用
      if (length(self$memory_usage) >= 10) {
        self$memory_usage <- self$memory_usage[-1]
      }
      self$memory_usage <- c(self$memory_usage, self$get_memory_usage())
      
      # 检查是否需要显示进度
      if (self$should_update_display()) {
        self$print_detailed_status()
      }
    },
    
    get_current_speed = function() {
      if (length(self$processing_speeds) == 0) return(0)
      return(mean(self$processing_speeds, na.rm = TRUE))
    },
    
    get_memory_usage = function() {
      # 获取当前R进程的内存使用（MB）
      tryCatch({
        gc_info <- gc(verbose = FALSE)
        return(sum(gc_info[, 2]))  # 返回已使用的内存
      }, error = function(e) {
        return(0)
      })
    },
    
    get_eta = function() {
      current_speed <- self$get_current_speed()
      if (current_speed <= 0 || self$processed_rows == 0) return("计算中...")
      
      remaining_rows <- self$total_rows - self$processed_rows
      eta_seconds <- remaining_rows / current_speed
      
      if (eta_seconds < 60) {
        return(sprintf("%.0f秒", eta_seconds))
      } else if (eta_seconds < 3600) {
        return(sprintf("%.1f分钟", eta_seconds / 60))
      } else {
        hours <- floor(eta_seconds / 3600)
        minutes <- round((eta_seconds %% 3600) / 60)
        return(sprintf("%d小时%d分钟", hours, minutes))
      }
    },
    
    should_update_display = function() {
      return(self$processed_rows %% GLOBAL_CONFIG$PROGRESS_UPDATE_INTERVAL == 0 || 
             self$processed_rows == self$total_rows)
    },
    
    print_detailed_status = function() {
      if (!self$should_update_display()) return()
      
      current_time <- Sys.time()
      elapsed_time <- difftime(current_time, self$start_time, units = "mins")
      success_rate <- round(self$last_success_rate * 100, 1)
      progress_pct <- round(self$processed_rows / self$total_rows * 100, 1)
      current_speed <- self$get_current_speed()
      avg_memory <- mean(self$memory_usage, na.rm = TRUE)
      
      # 清屏并重新显示（保持在同一位置）
      cat("\r" %R% strrep(" ", 100) %R% "\r")  # 清除当前行
      
      # 简单文本进度条
      bar_width <- 50
      filled_width <- round(bar_width * progress_pct / 100)
      progress_bar <- paste0("[", 
                            strrep("█", filled_width), 
                            strrep("░", bar_width - filled_width), 
                            "]")
      
      # 主进度显示
      cat(sprintf("\r%s %.1f%%", progress_bar, progress_pct))
      
      # 详细统计信息（多行显示）
      cat("\n┌" %R% strrep("─", 78) %R% "┐\n")
      cat(sprintf("│ 📊 处理进度: %s / %s 条 (%.1f%%)%s│\n", 
                  format(self$processed_rows, big.mark = ","),
                  format(self$total_rows, big.mark = ","),
                  progress_pct,
                  strrep(" ", 78 - nchar(sprintf("📊 处理进度: %s / %s 条 (%.1f%%)", 
                                                format(self$processed_rows, big.mark = ","),
                                                format(self$total_rows, big.mark = ","),
                                                progress_pct)) - 3)))
      
      cat(sprintf("│ ✅ 成功率: %.1f%% (%s成功, %s失败)%s│\n", 
                  success_rate,
                  format(self$successful_rows, big.mark = ","),
                  format(self$failed_rows, big.mark = ","),
                  strrep(" ", 78 - nchar(sprintf("✅ 成功率: %.1f%% (%s成功, %s失败)", 
                                                success_rate,
                                                format(self$successful_rows, big.mark = ","),
                                                format(self$failed_rows, big.mark = ","))) - 3)))
      
      cat(sprintf("│ ⚡ 当前速度: %.1f 条/秒 | ⏱️ 剩余时间: %s%s│\n", 
                  current_speed,
                  self$get_eta(),
                  strrep(" ", 78 - nchar(sprintf("⚡ 当前速度: %.1f 条/秒 | ⏱️ 剩余时间: %s", 
                                                current_speed, self$get_eta())) - 3)))
      
      cat(sprintf("│ 💾 内存使用: %.1f MB | ⏲️ 已运行: %.1f 分钟%s│\n", 
                  avg_memory,
                  as.numeric(elapsed_time),
                  strrep(" ", 78 - nchar(sprintf("💾 内存使用: %.1f MB | ⏲️ 已运行: %.1f 分钟", 
                                                avg_memory, as.numeric(elapsed_time))) - 3)))
      
      # 连续失败警告
      if (self$consecutive_failures > 10) {
        cat(sprintf("│ ⚠️  警告: 连续失败 %d 条记录%s│\n", 
                    self$consecutive_failures,
                    strrep(" ", 78 - nchar(sprintf("⚠️  警告: 连续失败 %d 条记录", 
                                                  self$consecutive_failures)) - 3)))
      }
      
      cat("└" %R% strrep("─", 78) %R% "┘\n")
      flush.console()
    },
    
    print_final_summary = function(processing_time) {
      cat("\n\n" %R% strrep("=", 80) %R% "\n")
      cat("🎉 地理编码处理完成!\n")
      cat(strrep("=", 80) %R% "\n")
      cat(sprintf("📋 总处理数据: %s 条\n", format(self$total_rows, big.mark = ",")))
      cat(sprintf("✅ 成功编码: %s 条 (%.1f%%)\n", 
                  format(self$successful_rows, big.mark = ","),
                  self$successful_rows / self$total_rows * 100))
      cat(sprintf("❌ 失败记录: %s 条 (%.1f%%)\n", 
                  format(self$failed_rows, big.mark = ","),
                  self$failed_rows / self$total_rows * 100))
      cat(sprintf("⏱️ 总处理时间: %.1f 分钟\n", as.numeric(processing_time)))
      cat(sprintf("⚡ 平均处理速度: %.1f 条/分钟\n", 
                  self$total_rows / as.numeric(processing_time)))
      cat(sprintf("💾 峰值内存使用: %.1f MB\n", max(self$memory_usage, na.rm = TRUE)))
      cat(strrep("=", 80) %R% "\n")
    }
  )
)

# 临时文件管理（简化版）
create_temp_folder <- function() {
  temp_folder <- paste0(GLOBAL_CONFIG$TEMP_FOLDER_PREFIX, format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir_create(temp_folder)
  return(temp_folder)
}

cleanup_temp_folder <- function(temp_folder) {
  if (dir_exists(temp_folder)) {
    tryCatch({
      dir_delete(temp_folder)
    }, error = function(e) {})
  }
}

# 批处理函数（增强版）
process_batch_final <- function(input_file, start_row, end_row, detected_fields, temp_folder, batch_num, monitor) {
  tryCatch({
    all_data <- read_excel(input_file)
    actual_start <- max(1, start_row)
    actual_end <- min(end_row, nrow(all_data))
    
    if (actual_start > nrow(all_data)) return(NULL)
    
    batch_data <- all_data[actual_start:actual_end, ]
    if (nrow(batch_data) == 0) return(NULL)
    
    # 逐条处理并更新进度
    batch_results <- vector("list", nrow(batch_data))
    
    for (i in 1:nrow(batch_data)) {
      result <- geocode_with_enhanced_fallback_final(detected_fields, batch_data[i, ])
      batch_results[[i]] <- list(
        latitude = result$lat,
        longitude = result$lon,
        confidence = result$confidence,
        geocoding_method = result$method,
        fields_used = ifelse(is.null(result$fields_used), "None", result$fields_used)
      )
      
      # 每处理一条记录就更新监控器
      monitor$update(result$success, 1)
    }
    
    batch_results_df <- bind_rows(batch_results)
    batch_with_data <- bind_cols(batch_data, batch_results_df)
    
    temp_file <- file.path(temp_folder, paste0("batch_", sprintf("%04d", batch_num), ".xlsx"))
    write_xlsx(batch_with_data, temp_file)
    
    return(list(
      file = temp_file,
      rows_processed = nrow(batch_data),
      rows_successful = sum(!is.na(batch_results_df$latitude))
    ))
    
  }, error = function(e) {
    cat("批次处理错误:", e$message, "\n")
    return(NULL)
  })
}

# 合并结果
merge_temp_files <- function(temp_files, output_file, original_data, detected_fields) {
  cat("\n正在合并处理结果...\n")
  
  all_results <- map_dfr(temp_files, function(temp_file) {
    if (file_exists(temp_file)) {
      return(read_excel(temp_file))
    }
    return(NULL)
  })
  
  if (nrow(all_results) == 0) {
    stop("没有找到有效的处理结果")
  }
  
  final_results <- reorder_output_columns(all_results, original_data, detected_fields)
  write_xlsx(final_results, output_file)
  
  return(final_results)
}

# 主运行函数（增强版）
run_enhanced_geocoding <- function() {
  cat("=== 大规模地理编码处理系统（增强进度版）===\n")
  cat("基于修复后的字段映射和优化策略\n")
  cat("✨ 新功能: 增强的进度显示和系统监控\n\n")
  
  # 系统检查
  check_system_requirements()
  
  # 文件选择
  input_file <- select_file_gui("选择Excel输入文件")
  output_file <- select_save_location_gui("geocoded_results_enhanced.xlsx")
  
  # 创建临时文件夹
  temp_folder <- create_temp_folder()
  
  # 分析文件
  cat("正在分析输入文件...\n")
  sample_data <- read_excel(input_file, n_max = 10)
  detected_fields <- detect_address_fields_final(names(sample_data))
  
  # 显示检测到的字段
  cat("\n🔍 检测到的地址字段:\n")
  field_count <- 0
  for (field_type in names(detected_fields)) {
    field_name <- detected_fields[[field_type]]
    if (!is.null(field_name)) {
      cat(sprintf("  ✅ %s: %s\n", field_type, field_name))
      field_count <- field_count + 1
    }
  }
  
  if (field_count == 0) {
    stop("❌ 未找到可识别的地址字段")
  }
  
  # 获取总行数
  total_rows <- nrow(read_excel(input_file))
  cat(sprintf("\n📊 总行数: %s 行\n", format(total_rows, big.mark = ",")))
  cat(sprintf("📈 预估成功率: 80-90%% （基于测试结果）\n"))
  cat(sprintf("⚙️ 批次大小: %d 条/批次\n", GLOBAL_CONFIG$BATCH_SIZE))
  cat(sprintf("📢 进度更新频率: 每 %d 条记录\n", GLOBAL_CONFIG$PROGRESS_UPDATE_INTERVAL))
  
  # 确认开始处理
  cat("\n按 Enter 键开始处理，或输入 'q' 退出: ")
  user_input <- readline()
  if (tolower(str_trim(user_input)) == "q") {
    cat("处理已取消\n")
    cleanup_temp_folder(temp_folder)
    return(NULL)
  }
  
  # 初始化增强监控
  monitor <- EnhancedProgressMonitor$new(total_rows)
  temp_files <- c()
  current_batch <- 1
  current_row <- 1
  
  start_time <- Sys.time()
  
  # 处理循环
  while (current_row <= total_rows) {
    end_row <- min(current_row + GLOBAL_CONFIG$BATCH_SIZE - 1, total_rows)
    batch_size <- end_row - current_row + 1
    
    batch_result <- process_batch_final(input_file, current_row, end_row, 
                                      detected_fields, temp_folder, current_batch, monitor)
    
    if (!is.null(batch_result)) {
      temp_files <- c(temp_files, batch_result$file)
      
      # 检查是否需要警告或中止
      if (monitor$consecutive_failures > GLOBAL_CONFIG$MAX_CONSECUTIVE_FAILURES) {
        cat("\n⚠️ 警告: 连续失败次数过多，是否继续？(y/N): ")
        user_choice <- readline()
        if (tolower(str_trim(user_choice)) != "y") {
          cat("处理已中止\n")
          break
        }
        monitor$consecutive_failures <- 0  # 重置计数器
      }
      
      # 定期保存进度
      if (current_batch %% 10 == 0) {
        tryCatch({
          progress_data <- list(
            current_batch = current_batch,
            current_row = current_row,
            processed_rows = monitor$processed_rows,
            successful_rows = monitor$successful_rows,
            temp_files = temp_files,
            timestamp = Sys.time()
          )
          saveRDS(progress_data, file.path(temp_folder, GLOBAL_CONFIG$PROGRESS_FILE))
        }, error = function(e) {
          # 静默处理保存错误
        })
      }
    } else {
      # 批次处理失败 - 手动更新失败的记录数
      for (i in 1:batch_size) {
        monitor$update(FALSE, 1)
      }
    }
    
    current_row <- end_row + 1
    current_batch <- current_batch + 1
  }
  
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  
  # 合并结果
  cat("\n\n🔄 正在合并最终结果...\n")
  original_data <- read_excel(input_file, n_max = 1)
  
  tryCatch({
    final_results <- merge_temp_files(temp_files, output_file, original_data, detected_fields)
    
    # 显示最终统计
    monitor$print_final_summary(processing_time)
    
    # 质量分析
    cat("\n📈 结果质量分析:\n")
    cat("┌────────────────────────────────────┐\n")
    confidence_stats <- final_results %>% 
      count(confidence, sort = TRUE) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    for (i in 1:nrow(confidence_stats)) {
      cat(sprintf("│ %-15s: %6d 条 (%5.1f%%) │\n", 
                  confidence_stats$confidence[i], 
                  confidence_stats$n[i],
                  confidence_stats$percentage[i]))
    }
    cat("└────────────────────────────────────┘\n")
    
    cat(sprintf("\n💾 结果已保存到: %s\n", output_file))
    cat(sprintf("📁 临时文件夹: %s (将被自动清理)\n", temp_folder))
    
    # 清理临时文件
    cat("\n🧹 正在清理临时文件...\n")
    cleanup_temp_folder(temp_folder)
    
    return(list(
      status = "completed",
      results = final_results,
      stats = list(
        total_rows = nrow(final_results),
        successful_rows = monitor$successful_rows,
        success_rate = monitor$last_success_rate,
        processing_time = processing_time,
        output_file = output_file,
        avg_speed = monitor$get_current_speed()
      )
    ))
    
  }, error = function(e) {
    cat(sprintf("\n❌ 处理过程中出现错误: %s\n", e$message))
    cat("🔧 尝试从临时文件恢复部分结果...\n")
    
    # 尝试恢复部分结果
    partial_results <- tryCatch({
      temp_excel_files <- list.files(temp_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(temp_excel_files) > 0) {
        partial_data <- map_dfr(temp_excel_files, read_excel)
        partial_output <- str_replace(output_file, "\\.xlsx$", "_partial.xlsx")
        write_xlsx(partial_data, partial_output)
        cat(sprintf("📄 部分结果已保存到: %s\n", partial_output))
        return(partial_data)
      }
      return(NULL)
    }, error = function(e2) NULL)
    
    monitor$print_final_summary(processing_time)
    cleanup_temp_folder(temp_folder)
    
    return(list(
      status = "partial",
      results = partial_results,
      error = e$message,
      stats = list(
        processed_rows = monitor$processed_rows,
        successful_rows = monitor$successful_rows,
        processing_time = processing_time
      )
    ))
  })
}

# 字符串连接操作符（用于格式化输出）
`%R%` <- function(x, y) paste0(x, y)

cat("🎉 增强版地理编码系统已准备就绪!\n")
cat("✨ 新功能特性:\n")
cat("  📊 实时进度条和详细统计\n")
cat("  ⚡ 动态速度计算（基于最近100条记录）\n")
cat("  💾 内存使用监控\n")
cat("  🚨 连续失败预警\n")
cat("  📈 成功率实时显示\n")
cat("  ⏱️ 智能剩余时间估算\n")
cat("  🔄 每100条记录更新一次进度\n")
cat("  📋 最终详细质量分析报告\n\n")
cat("🚀 运行命令: results <- run_enhanced_geocoding()\n")