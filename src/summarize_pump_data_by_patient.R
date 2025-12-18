# ============================================
# summarize_pump_data_by_patient.R
# Purpose: Summarize pump data files by patient
# Summarizes: Pump_BasalRateChange, Pump_BolusDelivered, Pump_CGMGlucoseValue
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

# Helper function to safely parse datetime
parse_datetime_safe <- function(x) {
  # If already a POSIXct/POSIXt, return as is
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(x)
  }
  
  x_char <- as.character(x)
  x_char[x_char == "" | is.na(x_char)] <- NA_character_
  result <- ymd_hms(x_char, quiet = TRUE)
  failed <- is.na(result) & !is.na(x_char)
  if (any(failed)) {
    result[failed] <- suppressWarnings(as.POSIXct(x_char[failed], optional = TRUE))
  }
  return(result)
}

cat("========================================\n")
cat("Summarizing Pump Data by Patient\n")
cat("========================================\n\n")

# ============================================
# 1. Summarize Basal Rate Changes
# ============================================
cat("1. Processing Basal Rate Changes...\n")
basal_file <- file.path(data_dir, "Pump_BasalRateChange.txt")
basal_data <- read_data_file(basal_file)

if (!is.null(basal_data)) {
  cat("   Loaded", nrow(basal_data), "records\n")
  
  # Parse datetime
  basal_data <- basal_data %>%
    mutate(
      DataDtTm = parse_datetime_safe(DataDtTm),
      DataDtTm_adjusted = parse_datetime_safe(DataDtTm_adjusted),
      Date = as.Date(DataDtTm)  # Extract date directly from DataDtTm
    ) %>%
    filter(!is.na(PtID), !is.na(CommandedBasalRate))
  
  # Summarize by patient
  basal_summary <- basal_data %>%
    group_by(PtID) %>%
    summarise(
      # Count metrics
      n_basal_changes = n(),
      n_days_with_data = n_distinct(Date, na.rm = TRUE),
      
      # Basal rate statistics
      basal_rate_mean = mean(CommandedBasalRate, na.rm = TRUE),
      basal_rate_median = median(CommandedBasalRate, na.rm = TRUE),
      basal_rate_sd = sd(CommandedBasalRate, na.rm = TRUE),
      basal_rate_min = min(CommandedBasalRate, na.rm = TRUE),
      basal_rate_max = max(CommandedBasalRate, na.rm = TRUE),
      basal_rate_iqr = IQR(CommandedBasalRate, na.rm = TRUE),
      
      # Non-zero basal rates (active periods)
      n_nonzero_basal = sum(CommandedBasalRate > 0, na.rm = TRUE),
      mean_nonzero_basal = mean(CommandedBasalRate[CommandedBasalRate > 0], na.rm = TRUE),
      
      # Date range
      first_date = min(Date, na.rm = TRUE),
      last_date = max(Date, na.rm = TRUE),
      date_range_days = as.numeric(difftime(max(Date, na.rm = TRUE), 
                                            min(Date, na.rm = TRUE), units = "days")),
      
      .groups = "drop"
    ) %>%
    mutate(
      basal_rate_cv = ifelse(basal_rate_mean > 0, 
                             (basal_rate_sd / basal_rate_mean) * 100, 
                             NA_real_)
    )
  
  cat("   Summarized", nrow(basal_summary), "patients\n\n")
} else {
  basal_summary <- NULL
  cat("   Warning: Could not load basal rate data\n\n")
}

# ============================================
# 2. Summarize Bolus Deliveries
# ============================================
cat("2. Processing Bolus Deliveries...\n")
bolus_file <- file.path(data_dir, "Pump_BolusDelivered.txt")
bolus_data <- read_data_file(bolus_file)

if (!is.null(bolus_data)) {
  cat("   Loaded", nrow(bolus_data), "records\n")
  
  # Parse datetime
  bolus_data <- bolus_data %>%
    mutate(
      DataDtTm = parse_datetime_safe(DataDtTm),
      DataDtTm_adjusted = parse_datetime_safe(DataDtTm_adjusted),
      Date = as.Date(DataDtTm)  # Extract date directly from DataDtTm
    ) %>%
    filter(!is.na(PtID), !is.na(BolusAmount))
  
  # Summarize by patient
  bolus_summary <- bolus_data %>%
    group_by(PtID) %>%
    summarise(
      # Count metrics
      n_bolus_deliveries = n(),
      n_days_with_data = n_distinct(Date, na.rm = TRUE),
      
      # Bolus amount statistics
      bolus_total = sum(BolusAmount, na.rm = TRUE),
      bolus_mean = mean(BolusAmount, na.rm = TRUE),
      bolus_median = median(BolusAmount, na.rm = TRUE),
      bolus_sd = sd(BolusAmount, na.rm = TRUE),
      bolus_min = min(BolusAmount, na.rm = TRUE),
      bolus_max = max(BolusAmount, na.rm = TRUE),
      bolus_iqr = IQR(BolusAmount, na.rm = TRUE),
      
      # Daily bolus metrics
      bolus_per_day = bolus_total / n_days_with_data,
      bolus_frequency_per_day = n_bolus_deliveries / n_days_with_data,
      
      # Bolus type distribution
      n_standard_bolus = sum(BolusType == "Standard", na.rm = TRUE),
      n_extended_bolus = sum(BolusType == "Extended", na.rm = TRUE),
      n_dual_bolus = sum(BolusType == "Dual", na.rm = TRUE),
      pct_standard = (n_standard_bolus / n_bolus_deliveries) * 100,
      
      # Date range
      first_date = min(Date, na.rm = TRUE),
      last_date = max(Date, na.rm = TRUE),
      date_range_days = as.numeric(difftime(max(Date, na.rm = TRUE), 
                                            min(Date, na.rm = TRUE), units = "days")),
      
      .groups = "drop"
    ) %>%
    mutate(
      bolus_cv = ifelse(bolus_mean > 0, 
                       (bolus_sd / bolus_mean) * 100, 
                       NA_real_)
    )
  
  cat("   Summarized", nrow(bolus_summary), "patients\n\n")
} else {
  bolus_summary <- NULL
  cat("   Warning: Could not load bolus data\n\n")
}

# ============================================
# 3. Summarize CGM Glucose Values
# ============================================
cat("3. Processing CGM Glucose Values...\n")
cgm_file <- file.path(data_dir, "Pump_CGMGlucoseValue.txt")
cgm_data <- read_data_file(cgm_file)

if (!is.null(cgm_data)) {
  cat("   Loaded", nrow(cgm_data), "records\n")
  
  # Parse datetime
  cgm_data <- cgm_data %>%
    mutate(
      DataDtTm = parse_datetime_safe(DataDtTm),
      DataDtTm_adjusted = parse_datetime_safe(DataDtTm_adjusted),
      Date = as.Date(DataDtTm)  # Extract date directly from DataDtTm
    ) %>%
    filter(!is.na(PtID), !is.na(CGMValue), CGMValue > 0)  # Filter out invalid readings
  
  cat("   Processing CGM summaries by patient (this may take a while)...\n")
  
  # Summarize by patient (basic metrics)
  cgm_summary <- cgm_data %>%
    group_by(PtID) %>%
    summarise(
      # Count metrics
      n_cgm_readings = n(),
      n_days_with_data = n_distinct(Date, na.rm = TRUE),
      readings_per_day = n() / n_days_with_data,
      
      # Glucose statistics
      glucose_mean = mean(CGMValue, na.rm = TRUE),
      glucose_median = median(CGMValue, na.rm = TRUE),
      glucose_sd = sd(CGMValue, na.rm = TRUE),
      glucose_min = min(CGMValue, na.rm = TRUE),
      glucose_max = max(CGMValue, na.rm = TRUE),
      glucose_iqr = IQR(CGMValue, na.rm = TRUE),
      
      # Time-in-Range metrics
      n_below_70 = sum(CGMValue < 70, na.rm = TRUE),
      n_70_180 = sum(CGMValue >= 70 & CGMValue <= 180, na.rm = TRUE),
      n_above_180 = sum(CGMValue > 180, na.rm = TRUE),
      n_above_250 = sum(CGMValue > 250, na.rm = TRUE),
      
      pct_below_70 = (n_below_70 / n_cgm_readings) * 100,
      pct_70_180 = (n_70_180 / n_cgm_readings) * 100,
      pct_above_180 = (n_above_180 / n_cgm_readings) * 100,
      pct_above_250 = (n_above_250 / n_cgm_readings) * 100,
      
      # High/Low indicators
      n_high_indicator = sum(HighLowIndicator > 0, na.rm = TRUE),
      n_low_indicator = sum(HighLowIndicator < 0, na.rm = TRUE),
      
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
    iglu_metrics <- calculate_iglu_metrics(cgm_data, 
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
} else {
  cgm_summary <- NULL
  cat("   Warning: Could not load CGM data\n\n")
}

# ============================================
# 4. Combine summaries
# ============================================
cat("4. Combining summaries...\n")

# Get all unique patient IDs
all_ptids <- unique(c(
  if (!is.null(basal_summary)) basal_summary$PtID else NULL,
  if (!is.null(bolus_summary)) bolus_summary$PtID else NULL,
  if (!is.null(cgm_summary)) cgm_summary$PtID else NULL
))

# Create combined summary
combined_summary <- data.frame(PtID = all_ptids, stringsAsFactors = FALSE)

# Merge each summary
if (!is.null(basal_summary)) {
  combined_summary <- combined_summary %>%
    left_join(basal_summary, by = "PtID")
}

if (!is.null(bolus_summary)) {
  combined_summary <- combined_summary %>%
    left_join(bolus_summary, by = "PtID")
}

if (!is.null(cgm_summary)) {
  combined_summary <- combined_summary %>%
    left_join(cgm_summary, by = "PtID")
}

cat("   Combined summary includes", nrow(combined_summary), "patients\n\n")

# ============================================
# 5. Save outputs
# ============================================
cat("5. Saving outputs...\n")

output_dir <- here("output", "tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save individual summaries
if (!is.null(basal_summary)) {
  write_csv(basal_summary, 
            file.path(output_dir, "basal_rate_summary_by_patient.csv"))
  cat("   Saved: basal_rate_summary_by_patient.csv\n")
}

if (!is.null(bolus_summary)) {
  write_csv(bolus_summary, 
            file.path(output_dir, "bolus_delivery_summary_by_patient.csv"))
  cat("   Saved: bolus_delivery_summary_by_patient.csv\n")
}

if (!is.null(cgm_summary)) {
  write_csv(cgm_summary, 
            file.path(output_dir, "cgm_summary_by_patient.csv"))
  cat("   Saved: cgm_summary_by_patient.csv\n")
}

# Save combined summary
write_csv(combined_summary, 
          file.path(output_dir, "pump_data_combined_summary_by_patient.csv"))
cat("   Saved: pump_data_combined_summary_by_patient.csv\n\n")

# ============================================
# 6. Print summary statistics
# ============================================
cat("========================================\n")
cat("Summary Statistics\n")
cat("========================================\n\n")

if (!is.null(basal_summary)) {
  cat("Basal Rate Changes:\n")
  cat("  Patients:", nrow(basal_summary), "\n")
  cat("  Total changes:", sum(basal_summary$n_basal_changes, na.rm = TRUE), "\n")
  cat("  Mean changes per patient:", 
      round(mean(basal_summary$n_basal_changes, na.rm = TRUE), 1), "\n")
  cat("  Mean basal rate:", 
      round(mean(basal_summary$basal_rate_mean, na.rm = TRUE), 2), "U/hr\n\n")
}

if (!is.null(bolus_summary)) {
  cat("Bolus Deliveries:\n")
  cat("  Patients:", nrow(bolus_summary), "\n")
  cat("  Total boluses:", sum(bolus_summary$n_bolus_deliveries, na.rm = TRUE), "\n")
  cat("  Mean boluses per patient:", 
      round(mean(bolus_summary$n_bolus_deliveries, na.rm = TRUE), 1), "\n")
  cat("  Mean bolus amount:", 
      round(mean(bolus_summary$bolus_mean, na.rm = TRUE), 2), "U\n\n")
}

if (!is.null(cgm_summary)) {
  cat("CGM Glucose Values:\n")
  cat("  Patients:", nrow(cgm_summary), "\n")
  cat("  Total readings:", sum(cgm_summary$n_cgm_readings, na.rm = TRUE), "\n")
  cat("  Mean readings per patient:", 
      round(mean(cgm_summary$n_cgm_readings, na.rm = TRUE), 0), "\n")
  cat("  Mean glucose:", 
      round(mean(cgm_summary$glucose_mean, na.rm = TRUE), 1), "mg/dL\n")
  cat("  Mean time-in-range (70-180):", 
      round(mean(cgm_summary$pct_70_180, na.rm = TRUE), 1), "%\n\n")
}

cat("========================================\n")
cat("Summary complete!\n")
cat("========================================\n")

