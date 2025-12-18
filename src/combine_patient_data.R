# Combine TDD, Demographics, and CGM data for patients in TDD summary
# This script merges data from three CSV files based on patient IDs present in TDD summary

# Load required libraries
library(dplyr)
library(readr)

# Define file paths
tdd_file <- "output/tables/tdd_summary_by_patient.csv"
demographics_file <- "output/tables/patient_demographics.csv"
cgm_file <- "output/tables/cgm_summary_by_patient.csv"
output_file <- "output/tables/combined_patient_data.csv"

# Read the CSV files
cat("Reading TDD summary data...\n")
tdd_data <- read_csv(tdd_file, show_col_types = FALSE)

cat("Reading patient demographics data...\n")
demographics_data <- read_csv(demographics_file, show_col_types = FALSE)

cat("Reading CGM summary data...\n")
cgm_data <- read_csv(cgm_file, show_col_types = FALSE)

# Get list of patient IDs from TDD summary
tdd_ptids <- unique(tdd_data$PtID)
cat(sprintf("Found %d unique patient IDs in TDD summary\n", length(tdd_ptids)))

# Filter demographics and CGM data to only include patients in TDD summary
demographics_filtered <- demographics_data %>%
  filter(PtID %in% tdd_ptids)

cgm_filtered <- cgm_data %>%
  filter(PtID %in% tdd_ptids)

cat(sprintf("Demographics: %d patients matched\n", nrow(demographics_filtered)))
cat(sprintf("CGM: %d patients matched\n", nrow(cgm_filtered)))

# Merge the datasets
# Start with TDD data as the base (left join)
cat("Merging datasets...\n")
combined_data <- tdd_data %>%
  left_join(demographics_filtered, by = "PtID") %>%
  left_join(cgm_filtered, by = "PtID")

# Check for any patients in TDD that don't have demographics or CGM data
missing_demographics <- sum(is.na(combined_data$AgeAtEnrollment))
missing_cgm <- sum(is.na(combined_data$n_cgm_readings))

if (missing_demographics > 0) {
  cat(sprintf("Warning: %d patients in TDD summary are missing demographics data\n", missing_demographics))
}

if (missing_cgm > 0) {
  cat(sprintf("Warning: %d patients in TDD summary are missing CGM data\n", missing_cgm))
}

# Add calculated columns
cat("Calculating TDD/kg and TDD/BMI...\n")
combined_data <- combined_data %>%
  mutate(
    TDD_per_kg = tdd_mean / Weight_kg,
    TDD_per_BMI = tdd_mean / BMI
  )

# Check for any missing values in the calculated columns
missing_weight <- sum(is.na(combined_data$Weight_kg))
missing_bmi <- sum(is.na(combined_data$BMI))
missing_tdd_per_kg <- sum(is.na(combined_data$TDD_per_kg))
missing_tdd_per_bmi <- sum(is.na(combined_data$TDD_per_BMI))

if (missing_weight > 0) {
  cat(sprintf("Warning: %d patients are missing weight data (TDD/kg cannot be calculated)\n", missing_weight))
}

if (missing_bmi > 0) {
  cat(sprintf("Warning: %d patients are missing BMI data (TDD/BMI cannot be calculated)\n", missing_bmi))
}

# Write the combined data to CSV
cat(sprintf("Writing combined data to %s...\n", output_file))
write_csv(combined_data, output_file)

cat(sprintf("Successfully created combined dataset with %d rows and %d columns\n", 
            nrow(combined_data), ncol(combined_data)))
cat(sprintf("Output file: %s\n", output_file))

