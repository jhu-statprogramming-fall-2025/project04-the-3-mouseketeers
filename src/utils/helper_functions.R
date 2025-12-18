# ============================================
# helper_functions.R
# Purpose: Reusable functions for data processing
# ============================================

library(dplyr)
library(purrr)
library(iglu)

#' Calculate Time-in-Range (TIR) percentage
#' @param cgm_values Vector of CGM readings
#' @param lower_bound Lower TIR threshold (default 70 mg/dL)
#' @param upper_bound Upper TIR threshold (default 180 mg/dL)
#' @return Numeric TIR percentage
calculate_tir <- function(cgm_values, lower_bound = 70, upper_bound = 180) {
  in_range <- cgm_values >= lower_bound & cgm_values <= upper_bound
  mean(in_range, na.rm = TRUE) * 100
}

#' Calculate CGM variability metrics
#' @param cgm_values Vector of CGM readings
#' @return Named list of variability metrics
calculate_cgm_variability <- function(cgm_values) {
  list(
    cgm_mean = mean(cgm_values, na.rm = TRUE),
    cgm_median = median(cgm_values, na.rm = TRUE),
    cgm_sd = sd(cgm_values, na.rm = TRUE),
    cgm_cv = sd(cgm_values, na.rm = TRUE) / mean(cgm_values, na.rm = TRUE) * 100,
    cgm_iqr = IQR(cgm_values, na.rm = TRUE)
  )
}

#' Calculate insulin delivery summary
#' @param basal_rates Vector of basal rate values
#' @param bolus_doses Vector of bolus dose values
#' @return Named list of insulin metrics
calculate_insulin_summary <- function(basal_rates, bolus_doses) {
  list(
    basal_mean = mean(basal_rates, na.rm = TRUE),
    basal_variability = sd(basal_rates, na.rm = TRUE),
    bolus_mean = mean(bolus_doses, na.rm = TRUE),
    bolus_frequency = sum(!is.na(bolus_doses)),
    total_daily_dose = sum(basal_rates, na.rm = TRUE) + sum(bolus_doses, na.rm = TRUE)
  )
}

#' Categorize patient based on TIR
#' @param tir Time-in-range percentage
#' @param threshold TIR threshold (default 70%)
#' @return Character: "good" or "challenging"
categorize_glycemic_control <- function(tir, threshold = 70) {
  if_else(tir >= threshold, "good", "challenging")
}

# ============================================
# iglu Integration Functions
# ============================================

#' Convert CGM data to iglu format
#' @param data Data frame with CGM data
#' @param id_col Column name for patient ID (default: "PtID")
#' @param time_col Column name for datetime (default: "DataDtTm")
#' @param gl_col Column name for glucose values (default: "CGMValue" or "CGM")
#' @return Data frame in iglu format (id, time, gl)
convert_to_iglu_format <- function(data, id_col = "PtID", time_col = "DataDtTm", gl_col = NULL) {
  # Auto-detect glucose column if not specified
  if (is.null(gl_col)) {
    if ("CGMValue" %in% names(data)) {
      gl_col <- "CGMValue"
    } else if ("CGM" %in% names(data)) {
      gl_col <- "CGM"
    } else {
      stop("Could not find glucose column. Please specify gl_col parameter.")
    }
  }
  
  # Convert to iglu format
  result <- data %>%
    select(id = !!sym(id_col), 
           time = !!sym(time_col), 
           gl = !!sym(gl_col)) %>%
    filter(!is.na(id), !is.na(time), !is.na(gl), gl > 0) %>%
    arrange(id, time)
  
  return(result)
}

#' Calculate comprehensive CGM metrics using iglu
#' @param data Data frame in iglu format (id, time, gl) or raw CGM data
#' @param id_col Column name for patient ID (if data not in iglu format)
#' @param time_col Column name for datetime (if data not in iglu format)
#' @param gl_col Column name for glucose values (if data not in iglu format)
#' @return Data frame with comprehensive CGM metrics by patient
calculate_iglu_metrics <- function(data, id_col = NULL, time_col = NULL, gl_col = NULL) {
  # Convert to iglu format if needed
  if (!all(c("id", "time", "gl") %in% names(data))) {
    if (is.null(id_col) || is.null(time_col) || is.null(gl_col)) {
      stop("Data not in iglu format. Please provide id_col, time_col, and gl_col parameters.")
    }
    data <- convert_to_iglu_format(data, id_col, time_col, gl_col)
  }
  
  # Calculate all iglu metrics
  metrics_list <- list()
  
  # Basic metrics
  tryCatch({
    metrics_list$mean_glu <- mean_glu(data)
  }, error = function(e) warning("Error calculating mean_glu: ", e$message))
  
  tryCatch({
    metrics_list$cv_glu <- cv_glu(data)
  }, error = function(e) warning("Error calculating cv_glu: ", e$message))
  
  tryCatch({
    metrics_list$gmi <- gmi(data)
  }, error = function(e) warning("Error calculating gmi: ", e$message))
  
  # Time-in-Range metrics using iglu functions
  tryCatch({
    metrics_list$tir <- in_range_percent(data)
  }, error = function(e) warning("Error calculating in_range_percent: ", e$message))
  
  tryCatch({
    metrics_list$above <- above_percent(data)
  }, error = function(e) warning("Error calculating above_percent: ", e$message))
  
  tryCatch({
    metrics_list$below <- below_percent(data)
  }, error = function(e) warning("Error calculating below_percent: ", e$message))
  
  # Variability metrics
  tryCatch({
    metrics_list$sd_glu <- sd_glu(data)
  }, error = function(e) warning("Error calculating sd_glu: ", e$message))
  
  tryCatch({
    metrics_list$j_index <- j_index(data)
  }, error = function(e) warning("Error calculating j_index: ", e$message))
  
  tryCatch({
    metrics_list$mage <- mage(data)
  }, error = function(e) warning("Error calculating mage: ", e$message))
  
  # Combine all metrics
  all_metrics <- metrics_list[[1]]
  if (length(metrics_list) > 1) {
    for (i in 2:length(metrics_list)) {
      if (!is.null(metrics_list[[i]]) && nrow(metrics_list[[i]]) > 0) {
        all_metrics <- all_metrics %>%
          left_join(metrics_list[[i]], by = "id")
      }
    }
  }
  
  return(all_metrics)
}

#' Calculate time-in-range using iglu (wrapper function)
#' @param data Data frame with CGM data
#' @param id_col Column name for patient ID
#' @param time_col Column name for datetime
#' @param gl_col Column name for glucose values
#' @return Data frame with TIR percentages by patient
calculate_tir_iglu <- function(data, id_col = "PtID", time_col = "DataDtTm", gl_col = NULL) {
  # Convert to iglu format
  iglu_data <- convert_to_iglu_format(data, id_col, time_col, gl_col)
  
  # Calculate TIR using iglu (returns 70-180 range by default)
  tir_result <- in_range_percent(iglu_data)
  
  return(tir_result)
}

