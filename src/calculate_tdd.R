# ============================================
# calculate_tdd.R
# Purpose: Calculate Total Daily Dose (TDD) of insulin from pump data
# Combines basal rate changes and bolus deliveries to calculate daily TDD
# ============================================

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(here)

# Source helper functions
source(here("src", "utils", "helper_functions.R"))

# Set data directory
data_dir <- here("data", "raw", "Data Files")
output_dir <- here("output")
figures_dir <- here("output", "figures")
tables_dir <- here("output", "tables")

# Create output directories if they don't exist
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

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

# Function to calculate daily basal insulin from rate changes
# For each day, integrates basal rate over time periods
calculate_daily_basal <- function(basal_data) {
  cat("   Calculating daily basal insulin...\n")
  
  # First, get the most recent rate before each day for each patient
  # This handles cases where the rate was set on a previous day
  basal_data_sorted <- basal_data %>%
    arrange(PtID, DataDtTm) %>%
    filter(!is.na(DataDtTm), !is.na(CommandedBasalRate))
  
  # Get all unique patient-date combinations
  all_dates <- basal_data_sorted %>%
    distinct(PtID, Date) %>%
    arrange(PtID, Date)
  
  # Process each patient-date combination
  daily_basal_list <- lapply(1:nrow(all_dates), function(idx) {
    ptid <- all_dates$PtID[idx]
    current_date <- all_dates$Date[idx]
    
    day_start <- as.POSIXct(paste0(current_date, " 00:00:00"))
    day_end <- as.POSIXct(paste0(current_date, " 23:59:59"))
    
    # Get all rate changes for this patient up to and including this day
    patient_rates <- basal_data_sorted %>%
      filter(PtID == ptid, DataDtTm <= day_end) %>%
      arrange(DataDtTm)
    
    if (nrow(patient_rates) == 0) {
      return(data.frame(PtID = ptid, Date = current_date, daily_basal_insulin = 0))
    }
    
    # Get rate changes that occur during this day
    day_rates <- patient_rates %>%
      filter(DataDtTm >= day_start, DataDtTm <= day_end) %>%
      arrange(DataDtTm)
    
    # Get the rate that was active at the start of the day (from previous day if needed)
    rates_before_day <- patient_rates %>%
      filter(DataDtTm < day_start) %>%
      arrange(DataDtTm)
    
    # Combine: start rate (if exists) + day rates
    if (nrow(rates_before_day) > 0) {
      start_rate <- rates_before_day$CommandedBasalRate[nrow(rates_before_day)]
      start_time <- day_start
    } else if (nrow(day_rates) > 0) {
      start_rate <- day_rates$CommandedBasalRate[1]
      start_time <- day_rates$DataDtTm[1]
    } else {
      return(data.frame(PtID = ptid, Date = current_date, daily_basal_insulin = 0))
    }
    
    # Build list of all rate periods for this day
    rate_periods <- list()
    
    # If first rate change is after day start, add period from day start
    if (nrow(day_rates) > 0 && day_rates$DataDtTm[1] > day_start) {
      rate_periods[[1]] <- list(
        rate = start_rate,
        start = day_start,
        end = day_rates$DataDtTm[1]
      )
    } else if (nrow(day_rates) == 0) {
      # No rate changes during day, use start rate for whole day
      rate_periods[[1]] <- list(
        rate = start_rate,
        start = day_start,
        end = day_end
      )
    }
    
    # Add periods for each rate change during the day
    if (nrow(day_rates) > 0) {
      for (i in 1:nrow(day_rates)) {
        period_start <- day_rates$DataDtTm[i]
        period_rate <- day_rates$CommandedBasalRate[i]
        
        # Determine end time
        if (i < nrow(day_rates)) {
          period_end <- day_rates$DataDtTm[i + 1]
        } else {
          period_end <- day_end
        }
        
        rate_periods[[length(rate_periods) + 1]] <- list(
          rate = period_rate,
          start = period_start,
          end = period_end
        )
      }
    }
    
    # Calculate total basal insulin
    total_basal <- 0
    for (period in rate_periods) {
      duration_hours <- as.numeric(difftime(period$end, period$start, units = "hours"))
      if (duration_hours > 0 && !is.na(period$rate) && period$rate >= 0) {
        total_basal <- total_basal + (period$rate * duration_hours)
      }
    }
    
    return(data.frame(PtID = ptid, Date = current_date, daily_basal_insulin = total_basal))
  })
  
  daily_basal <- bind_rows(daily_basal_list)
  
  return(daily_basal)
}

cat("========================================\n")
cat("Calculating Total Daily Dose (TDD)\n")
cat("========================================\n\n")

# ============================================
# 1. Load Basal Rate Data
# ============================================
cat("1. Loading basal rate data...\n")
basal_file <- file.path(data_dir, "Pump_BasalRateChange.txt")
basal_data <- read_data_file(basal_file)

if (is.null(basal_data) || nrow(basal_data) == 0) {
  stop("Could not load basal rate data")
}

cat("   Loaded", nrow(basal_data), "basal rate records\n")

# Parse datetime and prepare data
basal_data <- basal_data %>%
  mutate(
    DataDtTm = parse_datetime_safe(DataDtTm),
    DataDtTm_adjusted = parse_datetime_safe(DataDtTm_adjusted),
    Date = as.Date(DataDtTm)
  ) %>%
  filter(!is.na(PtID), !is.na(CommandedBasalRate), !is.na(Date))

  # Use adjusted time if available, otherwise use original
  basal_data <- basal_data %>%
    mutate(
      DataDtTm = if_else(!is.na(DataDtTm_adjusted), DataDtTm_adjusted, DataDtTm),
      Date = as.Date(DataDtTm)
    ) %>%
    # Filter to only include data from 2017 onwards
    filter(Date >= as.Date("2017-01-01"))

cat("   After filtering:", nrow(basal_data), "records\n\n")

# ============================================
# 2. Load Bolus Data
# ============================================
cat("2. Loading bolus data...\n")
bolus_file <- file.path(data_dir, "Pump_BolusDelivered.txt")
bolus_data <- read_data_file(bolus_file)

if (is.null(bolus_data) || nrow(bolus_data) == 0) {
  stop("Could not load bolus data")
}

cat("   Loaded", nrow(bolus_data), "bolus records\n")

# Parse datetime and prepare data
bolus_data <- bolus_data %>%
  mutate(
    DataDtTm = parse_datetime_safe(DataDtTm),
    DataDtTm_adjusted = parse_datetime_safe(DataDtTm_adjusted),
    Date = as.Date(DataDtTm)
  ) %>%
  filter(!is.na(PtID), !is.na(BolusAmount), !is.na(Date))

  # Use adjusted time if available, otherwise use original
  bolus_data <- bolus_data %>%
    mutate(
      DataDtTm = if_else(!is.na(DataDtTm_adjusted), DataDtTm_adjusted, DataDtTm),
      Date = as.Date(DataDtTm)
    ) %>%
    # Filter to only include data from 2017 onwards
    filter(Date >= as.Date("2017-01-01"))

cat("   After filtering:", nrow(bolus_data), "records\n\n")

# ============================================
# 3. Calculate Daily Basal Insulin
# ============================================
cat("3. Calculating daily basal insulin...\n")
daily_basal <- calculate_daily_basal(basal_data)
cat("   Calculated basal insulin for", nrow(daily_basal), "patient-days\n\n")

# ============================================
# 4. Calculate Daily Bolus Insulin
# ============================================
cat("4. Calculating daily bolus insulin...\n")
daily_bolus <- bolus_data %>%
  group_by(PtID, Date) %>%
  summarise(
    daily_bolus_insulin = sum(BolusAmount, na.rm = TRUE),
    n_boluses = n(),
    .groups = "drop"
  )

cat("   Calculated bolus insulin for", nrow(daily_bolus), "patient-days\n\n")

# ============================================
# 5. Calculate Daily TDD
# ============================================
cat("5. Calculating daily TDD...\n")

# Combine basal and bolus data
daily_tdd <- full_join(
  daily_basal,
  daily_bolus,
  by = c("PtID", "Date")
) %>%
  mutate(
    daily_basal_insulin = if_else(is.na(daily_basal_insulin), 0, daily_basal_insulin),
    daily_bolus_insulin = if_else(is.na(daily_bolus_insulin), 0, daily_bolus_insulin),
    n_boluses = if_else(is.na(n_boluses), 0, n_boluses),
    tdd = daily_basal_insulin + daily_bolus_insulin
  ) %>%
  filter(tdd > 0) %>%  # Only include days with some insulin delivery
  arrange(PtID, Date)

cat("   Calculated TDD for", nrow(daily_tdd), "patient-days\n\n")

# ============================================
# 6. Calculate Summary Statistics by Patient
# ============================================
cat("6. Calculating summary statistics by patient...\n")

tdd_summary_by_patient <- daily_tdd %>%
  group_by(PtID) %>%
  summarise(
    n_days = n(),
    first_date = min(Date, na.rm = TRUE),
    last_date = max(Date, na.rm = TRUE),
    date_range_days = as.numeric(difftime(max(Date, na.rm = TRUE), 
                                          min(Date, na.rm = TRUE), units = "days")) + 1,
    
    # TDD statistics
    tdd_mean = mean(tdd, na.rm = TRUE),
    tdd_median = median(tdd, na.rm = TRUE),
    tdd_sd = sd(tdd, na.rm = TRUE),
    tdd_min = min(tdd, na.rm = TRUE),
    tdd_max = max(tdd, na.rm = TRUE),
    tdd_iqr = IQR(tdd, na.rm = TRUE),
    tdd_q25 = quantile(tdd, 0.25, na.rm = TRUE),
    tdd_q75 = quantile(tdd, 0.75, na.rm = TRUE),
    
    # Basal statistics
    basal_mean = mean(daily_basal_insulin, na.rm = TRUE),
    basal_median = median(daily_basal_insulin, na.rm = TRUE),
    basal_sd = sd(daily_basal_insulin, na.rm = TRUE),
    
    # Bolus statistics
    bolus_mean = mean(daily_bolus_insulin, na.rm = TRUE),
    bolus_median = median(daily_bolus_insulin, na.rm = TRUE),
    bolus_sd = sd(daily_bolus_insulin, na.rm = TRUE),
    mean_boluses_per_day = mean(n_boluses, na.rm = TRUE),
    
    # Basal/Bolus ratio
    basal_pct = (basal_mean / tdd_mean) * 100,
    bolus_pct = (bolus_mean / tdd_mean) * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    tdd_cv = ifelse(tdd_mean > 0, (tdd_sd / tdd_mean) * 100, NA_real_)
  )

cat("   Summarized", nrow(tdd_summary_by_patient), "patients\n\n")

# ============================================
# 7. Calculate Overall Summary Statistics
# ============================================
cat("7. Calculating overall summary statistics...\n")

overall_summary <- daily_tdd %>%
  summarise(
    n_patients = n_distinct(PtID),
    n_patient_days = n(),
    
    # Overall TDD statistics
    overall_tdd_mean = mean(tdd, na.rm = TRUE),
    overall_tdd_median = median(tdd, na.rm = TRUE),
    overall_tdd_sd = sd(tdd, na.rm = TRUE),
    overall_tdd_min = min(tdd, na.rm = TRUE),
    overall_tdd_max = max(tdd, na.rm = TRUE),
    overall_tdd_iqr = IQR(tdd, na.rm = TRUE),
    overall_tdd_q25 = quantile(tdd, 0.25, na.rm = TRUE),
    overall_tdd_q75 = quantile(tdd, 0.75, na.rm = TRUE),
    
    # Overall basal statistics
    overall_basal_mean = mean(daily_basal_insulin, na.rm = TRUE),
    overall_basal_median = median(daily_basal_insulin, na.rm = TRUE),
    
    # Overall bolus statistics
    overall_bolus_mean = mean(daily_bolus_insulin, na.rm = TRUE),
    overall_bolus_median = median(daily_bolus_insulin, na.rm = TRUE),
    
    # Date range
    overall_first_date = min(Date, na.rm = TRUE),
    overall_last_date = max(Date, na.rm = TRUE)
  ) %>%
  mutate(
    overall_tdd_cv = ifelse(overall_tdd_mean > 0, 
                           (overall_tdd_sd / overall_tdd_mean) * 100, 
                           NA_real_)
  )

cat("   Overall statistics calculated\n\n")

# ============================================
# 8. Create Visualizations
# ============================================
cat("8. Creating visualizations...\n")

# 8.1 TDD distribution
p2 <- daily_tdd %>%
  ggplot(aes(x = tdd)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = overall_summary$overall_tdd_median), 
             color = "red", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = overall_summary$overall_tdd_mean), 
             color = "darkgreen", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Distribution of Total Daily Dose (TDD)",
    subtitle = paste0("Median = ", round(overall_summary$overall_tdd_median, 2), 
                     " U (red), Mean = ", round(overall_summary$overall_tdd_mean, 2), 
                     " U (green)"),
    x = "TDD (Units)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave(file.path(figures_dir, "tdd_distribution.png"), 
       p2, width = 10, height = 6, dpi = 300)
cat("   Saved: tdd_distribution.png\n")

# 8.2 TDD summary by patient (boxplot)
p3 <- daily_tdd %>%
  ggplot(aes(x = reorder(as.factor(PtID), tdd, median), y = tdd)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(aes(yintercept = overall_summary$overall_tdd_median), 
             color = "red", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Total Daily Dose (TDD) by Patient",
    subtitle = "Patients ordered by median TDD",
    x = "Patient ID",
    y = "TDD (Units)",
    caption = "Red line shows overall median TDD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )

ggsave(file.path(figures_dir, "tdd_by_patient_boxplot.png"), 
       p3, width = 14, height = 6, dpi = 300)
cat("   Saved: tdd_by_patient_boxplot.png\n")

# 8.3 Basal vs Bolus composition
p5 <- daily_tdd %>%
  sample_n(min(10000, nrow(daily_tdd))) %>%  # Sample for performance
  ggplot(aes(x = daily_basal_insulin, y = daily_bolus_insulin)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Daily Basal vs Bolus Insulin",
    subtitle = "Each point represents one patient-day",
    x = "Daily Basal Insulin (Units)",
    y = "Daily Bolus Insulin (Units)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave(file.path(figures_dir, "basal_vs_bolus_scatter.png"), 
       p5, width = 10, height = 8, dpi = 300)
cat("   Saved: basal_vs_bolus_scatter.png\n")

# 8.4 TDD over time - summarized (individual patient trajectories with smoothing)
p6 <- daily_tdd %>%
  ggplot(aes(x = Date, y = tdd, group = PtID)) +
  geom_line(alpha = 0.2, linewidth = 0.3, color = "gray60") +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, 
              color = "red", linewidth = 1.5, span = 0.3) +
  labs(
    title = "Total Daily Dose (TDD) Over Time",
    subtitle = "Individual patient trajectories (gray) with overall smoothed trend (red)",
    x = "Date",
    y = "TDD (Units)",
    caption = "Red line shows smoothed overall trend with 95% confidence interval"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(figures_dir, "tdd_over_time_summarized.png"), 
       p6, width = 12, height = 6, dpi = 300)
cat("   Saved: tdd_over_time_summarized.png\n")

# 8.5 TDD over time - aggregated by week
daily_tdd_weekly <- daily_tdd %>%
  mutate(
    Year = year(Date),
    Week = week(Date),
    YearWeek = paste0(Year, "-W", sprintf("%02d", Week))
  ) %>%
  group_by(YearWeek, Date) %>%
  summarise(
    week_start = min(Date),
    tdd_median = median(tdd, na.rm = TRUE),
    tdd_mean = mean(tdd, na.rm = TRUE),
    tdd_q25 = quantile(tdd, 0.25, na.rm = TRUE),
    tdd_q75 = quantile(tdd, 0.75, na.rm = TRUE),
    n_patients = n_distinct(PtID),
    .groups = "drop"
  ) %>%
  arrange(week_start)

p7 <- daily_tdd_weekly %>%
  ggplot(aes(x = week_start)) +
  geom_ribbon(aes(ymin = tdd_q25, ymax = tdd_q75), 
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = tdd_median), color = "red", linewidth = 1.5) +
  geom_line(aes(y = tdd_mean), color = "darkgreen", linewidth = 1, linetype = "dashed") +
  labs(
    title = "TDD Trend Over Time (Weekly Aggregated)",
    subtitle = "Median (red), Mean (green dashed), IQR (shaded)",
    x = "Date",
    y = "TDD (Units)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(figures_dir, "tdd_over_time_weekly.png"), 
       p7, width = 12, height = 6, dpi = 300)
cat("   Saved: tdd_over_time_weekly.png\n")

cat("   All visualizations created\n\n")

# ============================================
# 9. Save Outputs
# ============================================
cat("9. Saving outputs...\n")

# Save summary by patient
write_csv(tdd_summary_by_patient, file.path(tables_dir, "tdd_summary_by_patient.csv"))
cat("   Saved: tdd_summary_by_patient.csv\n")

# Save overall summary
write_csv(overall_summary, file.path(tables_dir, "tdd_overall_summary.csv"))
cat("   Saved: tdd_overall_summary.csv\n\n")

# ============================================
# 10. Print Summary
# ============================================
cat("========================================\n")
cat("Summary Statistics\n")
cat("========================================\n\n")

cat("Overall TDD Statistics:\n")
cat("  Patients:", overall_summary$n_patients, "\n")
cat("  Patient-days:", overall_summary$n_patient_days, "\n")
cat("  Median TDD:", round(overall_summary$overall_tdd_median, 2), "U\n")
cat("  Mean TDD:", round(overall_summary$overall_tdd_mean, 2), "U\n")
cat("  SD TDD:", round(overall_summary$overall_tdd_sd, 2), "U\n")
cat("  IQR TDD:", round(overall_summary$overall_tdd_iqr, 2), "U\n")
cat("  Range:", round(overall_summary$overall_tdd_min, 2), "-", 
    round(overall_summary$overall_tdd_max, 2), "U\n")
cat("  CV:", round(overall_summary$overall_tdd_cv, 2), "%\n\n")

cat("Basal vs Bolus:\n")
cat("  Mean Daily Basal:", round(overall_summary$overall_basal_mean, 2), "U\n")
cat("  Mean Daily Bolus:", round(overall_summary$overall_bolus_mean, 2), "U\n")
cat("  Basal % of TDD:", round((overall_summary$overall_basal_mean / overall_summary$overall_tdd_mean) * 100, 1), "%\n")
cat("  Bolus % of TDD:", round((overall_summary$overall_bolus_mean / overall_summary$overall_tdd_mean) * 100, 1), "%\n\n")

cat("Date Range:\n")
cat("  First date:", as.character(overall_summary$overall_first_date), "\n")
cat("  Last date:", as.character(overall_summary$overall_last_date), "\n\n")

cat("========================================\n")
cat("TDD calculation complete!\n")
cat("========================================\n")

