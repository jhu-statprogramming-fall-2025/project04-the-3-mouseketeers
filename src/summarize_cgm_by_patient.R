# ============================================
# summarize_cgm_by_patient.R
# Purpose: Summarize cgm.txt data by patient
# ============================================

library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(here)

# Source helper functions
source(here("src", "utils", "helper_functions.R"))

# Set data directory
data_dir <- here("data", "raw", "Data Files")

# Function to safely read a pipe-delimited file
read_data_file <- function(file_path) {
  tryCatch({
    first_line <- readLines(file_path, n = 1)
    if (grepl("\\|", first_line)) {
      df <- read_delim(file_path, delim = "|", show_col_types = FALSE, 
                       locale = locale(encoding = "UTF-8"), na = c("", "NA", " "))
      df <- df %>% mutate(across(where(is.character), ~na_if(.x, "")))
      return(df)
    } else {
      df <- read_tsv(file_path, show_col_types = FALSE,
                     locale = locale(encoding = "UTF-8"), na = c("", "NA", " "))
      df <- df %>% mutate(across(where(is.character), ~na_if(.x, "")))
      return(df)
    }
  }, error = function(e) {
    warning(paste("Error reading", basename(file_path), ":", e$message))
    return(NULL)
  })
}

# Helper function to parse the date format in cgm.txt (e.g., "11DEC17:23:59:25")
parse_cgm_datetime <- function(x) {
  x_char <- as.character(x)
  x_char[x_char == "" | is.na(x_char)] <- NA_character_
  
  # Try parsing with lubridate (handles various formats)
  result <- parse_date_time(x_char, orders = c("dmy HMS", "dmy_HMS", "ymd HMS", "ymd_HMS"), 
                            quiet = TRUE)
  
  # For any that failed, try as.POSIXct
  failed <- is.na(result) & !is.na(x_char)
  if (any(failed)) {
    result[failed] <- suppressWarnings(as.POSIXct(x_char[failed], optional = TRUE))
  }
  
  return(result)
}

cat("========================================\n")
cat("Summarizing cgm.txt Data by Patient\n")
cat("========================================\n\n")

# ============================================
# Load and process cgm.txt
# ============================================
cat("Processing cgm.txt...\n")
cgm_file <- file.path(data_dir, "cgm.txt")
cgm_data <- read_data_file(cgm_file)

if (!is.null(cgm_data)) {
  cat("   Loaded", nrow(cgm_data), "records\n")
  cat("   Columns:", paste(names(cgm_data), collapse = ", "), "\n")
  
  # Parse datetime and filter
  cgm_data <- cgm_data %>%
    mutate(
      DataDtTm = parse_cgm_datetime(DataDtTm),
      Date = as.Date(DataDtTm),
      CGM_numeric = as.numeric(CGM)
    ) %>%
    filter(!is.na(PtID), !is.na(CGM_numeric), CGM_numeric > 0)  # Filter out invalid readings
  
  cat("   After filtering:", nrow(cgm_data), "valid records\n")
  cat("   Processing summaries by patient (this may take a while)...\n")
  
  # Summarize by patient (basic metrics)
  cgm_summary <- cgm_data %>%
    group_by(PtID) %>%
    summarise(
      # Count metrics
      n_cgm_readings = n(),
      n_days_with_data = n_distinct(Date, na.rm = TRUE),
      readings_per_day = n() / n_days_with_data,
      
      # Period information
      n_periods = n_distinct(Period, na.rm = TRUE),
      periods = paste(unique(Period[!is.na(Period)]), collapse = "; "),
      
      # Glucose statistics
      glucose_mean = mean(CGM_numeric, na.rm = TRUE),
      glucose_median = median(CGM_numeric, na.rm = TRUE),
      glucose_sd = sd(CGM_numeric, na.rm = TRUE),
      glucose_min = min(CGM_numeric, na.rm = TRUE),
      glucose_max = max(CGM_numeric, na.rm = TRUE),
      glucose_iqr = IQR(CGM_numeric, na.rm = TRUE),
      
      # Time-in-Range metrics
      n_below_70 = sum(CGM_numeric < 70, na.rm = TRUE),
      n_70_180 = sum(CGM_numeric >= 70 & CGM_numeric <= 180, na.rm = TRUE),
      n_above_180 = sum(CGM_numeric > 180, na.rm = TRUE),
      n_above_250 = sum(CGM_numeric > 250, na.rm = TRUE),
      
      pct_below_70 = (n_below_70 / n_cgm_readings) * 100,
      pct_70_180 = (n_70_180 / n_cgm_readings) * 100,
      pct_above_180 = (n_above_180 / n_cgm_readings) * 100,
      pct_above_250 = (n_above_250 / n_cgm_readings) * 100,
      
      # Date range
      first_date = min(Date, na.rm = TRUE),
      last_date = max(Date, na.rm = TRUE),
      date_range_days = as.numeric(difftime(max(Date, na.rm = TRUE), 
                                            min(Date, na.rm = TRUE), units = "days")),
      
      .groups = "drop"
    ) %>%
    mutate(
      glucose_cv = ifelse(glucose_mean > 0, 
                          (glucose_sd / glucose_mean) * 100, 
                          NA_real_)
    )
  
  # Calculate iglu metrics
  cat("   Calculating iglu metrics...\n")
  tryCatch({
    # Create a temporary column with the numeric CGM values for iglu
    cgm_data_for_iglu <- cgm_data %>%
      mutate(CGMValue = CGM_numeric)
    
    iglu_metrics <- calculate_iglu_metrics(cgm_data_for_iglu, 
                                           id_col = "PtID", 
                                           time_col = "DataDtTm", 
                                           gl_col = "CGMValue")
    
    # Rename id column to PtID for merging
    if ("id" %in% names(iglu_metrics)) {
      iglu_metrics <- iglu_metrics %>%
        rename(PtID = id)
    }
    
    # Merge iglu metrics with basic summary
    cgm_summary <- cgm_summary %>%
      left_join(iglu_metrics, by = "PtID")
    
    cat("   Added iglu metrics successfully\n")
  }, error = function(e) {
    warning("   Could not calculate iglu metrics: ", e$message)
  })
  
  cat("   Summarized", nrow(cgm_summary), "patients\n\n")
  
  # ============================================
  # Save output
  # ============================================
  cat("Saving output...\n")
  
  output_dir <- here("output", "tables")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write_csv(cgm_summary, 
            file.path(output_dir, "cgm_txt_summary_by_patient.csv"))
  cat("   Saved: cgm_txt_summary_by_patient.csv\n\n")
  
  # ============================================
  # Print summary statistics
  # ============================================
  cat("========================================\n")
  cat("Summary Statistics\n")
  cat("========================================\n\n")
  
  cat("CGM Data (cgm.txt):\n")
  cat("  Patients:", nrow(cgm_summary), "\n")
  cat("  Total readings:", sum(cgm_summary$n_cgm_readings, na.rm = TRUE), "\n")
  cat("  Mean readings per patient:", 
      round(mean(cgm_summary$n_cgm_readings, na.rm = TRUE), 0), "\n")
  cat("  Mean glucose:", 
      round(mean(cgm_summary$glucose_mean, na.rm = TRUE), 1), "mg/dL\n")
  cat("  Median time-in-range (70-180):", 
      round(median(cgm_summary$pct_70_180, na.rm = TRUE), 1), "%\n")
  cat("  Mean time-in-range (70-180):", 
      round(mean(cgm_summary$pct_70_180, na.rm = TRUE), 1), "%\n\n")
  
  # Time-in-range analysis
  cat("Time-in-Range Analysis:\n")
  cat("  Patients >= 70% time in range:", 
      sum(cgm_summary$pct_70_180 >= 70, na.rm = TRUE), "\n")
  cat("  Patients < 70% time in range:", 
      sum(cgm_summary$pct_70_180 < 70, na.rm = TRUE), "\n")
  cat("  Median time in range:", 
      round(median(cgm_summary$pct_70_180, na.rm = TRUE), 1), "%\n")
  cat("  IQR:", 
      round(quantile(cgm_summary$pct_70_180, 0.25, na.rm = TRUE), 1), "-", 
      round(quantile(cgm_summary$pct_70_180, 0.75, na.rm = TRUE), 1), "%\n\n")
  
  cat("========================================\n")
  cat("Summary complete!\n")
  cat("========================================\n")
  
} else {
  cat("   Error: Could not load cgm.txt data\n")
}

