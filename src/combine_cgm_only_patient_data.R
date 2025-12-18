# Combine CGM and Demographics data for CGM-only patients
# This script merges CGM summary and demographics data for patients who only have CGM data
# (no insulin/pump data)

# Load required libraries
library(dplyr)
library(readr)

# Define file paths
cgm_only_ptids_file <- "data/processed/cgm_only_ptids.csv"
demographics_file <- "output/tables/patient_demographics.csv"
cgm_file <- "output/tables/cgm_txt_summary_by_patient.csv"
output_file <- "output/tables/combined_cgm_only_patient_data.csv"

# Read the CGM-only patient IDs
cat("Reading CGM-only patient IDs...\n")
cgm_only_ptids <- read_csv(cgm_only_ptids_file, show_col_types = FALSE)
cgm_only_ptids_list <- unique(cgm_only_ptids$PtID)
cgm_only_ptids_list <- cgm_only_ptids_list[!is.na(cgm_only_ptids_list)]  # Remove NA values
cat(sprintf("Found %d unique CGM-only patient IDs\n", length(cgm_only_ptids_list)))

# Read the CSV files
cat("Reading patient demographics data...\n")
demographics_data <- read_csv(demographics_file, show_col_types = FALSE)

cat("Reading CGM summary data...\n")
cgm_data <- read_csv(cgm_file, show_col_types = FALSE)

# Filter to only include CGM-only patients
cat("Filtering data to CGM-only patients...\n")
demographics_filtered <- demographics_data %>%
  filter(PtID %in% cgm_only_ptids_list)

cgm_filtered <- cgm_data %>%
  filter(PtID %in% cgm_only_ptids_list)

cat(sprintf("Demographics: %d patients matched\n", nrow(demographics_filtered)))
cat(sprintf("CGM: %d patients matched\n", nrow(cgm_filtered)))

# Check for patients in CGM-only list that don't have data
missing_demographics <- setdiff(cgm_only_ptids_list, demographics_filtered$PtID)
missing_cgm <- setdiff(cgm_only_ptids_list, cgm_filtered$PtID)

if (length(missing_demographics) > 0) {
  cat(sprintf("Warning: %d CGM-only patients are missing demographics data: %s\n", 
              length(missing_demographics), paste(missing_demographics, collapse = ", ")))
}

if (length(missing_cgm) > 0) {
  cat(sprintf("Warning: %d CGM-only patients are missing CGM data: %s\n", 
              length(missing_cgm), paste(missing_cgm, collapse = ", ")))
}

# Merge the datasets
# Start with CGM data as the base (left join)
cat("Merging datasets...\n")
combined_data <- cgm_filtered %>%
  left_join(demographics_filtered, by = "PtID")

# Reorder columns to match the structure of combined_patient_data.csv
# Order: PtID, CGM columns, Demographics columns
# (Excluding all TDD/insulin-related columns)

# Note: cgm_txt_summary_by_patient.csv has slightly different columns than cgm_summary_by_patient.csv
# It includes n_periods and periods, but doesn't have n_high_indicator and n_low_indicator

# Reorder to match the structure (excluding TDD columns)
combined_data <- combined_data %>%
  select(
    # Patient ID
    PtID,
    
    # CGM data columns (from cgm_txt_summary_by_patient.csv)
    n_cgm_readings,
    n_days_with_data,
    readings_per_day,
    n_periods,
    periods,
    glucose_mean,
    glucose_median,
    glucose_sd,
    glucose_min,
    glucose_max,
    glucose_iqr,
    n_below_70,
    n_70_180,
    n_above_180,
    n_above_250,
    pct_below_70,
    pct_70_180,
    pct_above_180,
    pct_above_250,
    first_date,
    last_date,
    date_range_days,
    glucose_cv,
    mean,
    CV,
    GMI,
    in_range_63_140,
    in_range_70_180,
    above_140,
    above_180,
    above_250,
    below_54,
    below_70,
    SD,
    J_index,
    MAGE,
    
    # Demographics columns (matching order from patient_demographics.csv)
    AgeAtEnrollment,
    Gender,
    Ethnicity,
    Race,
    DiagAge,
    DiagAgeApprox,
    Weight_kg,
    Height_cm,
    BMI,
    BldPrSys,
    BldPrDia,
    EducationLevel,
    AnnualIncome,
    InsuranceType,
    HbA1c_Screening,
    HbA1cTestDt,
    NumMedicalConditions,
    MedicalConditions,
    NumMedications,
    Medications
  )

# Check for any missing values
missing_demographics_count <- sum(is.na(combined_data$AgeAtEnrollment))
missing_cgm_count <- sum(is.na(combined_data$n_cgm_readings))

if (missing_demographics_count > 0) {
  cat(sprintf("Warning: %d patients are missing demographics data\n", missing_demographics_count))
}

if (missing_cgm_count > 0) {
  cat(sprintf("Warning: %d patients are missing CGM data\n", missing_cgm_count))
}

# Write the combined data to CSV
cat(sprintf("Writing combined data to %s...\n", output_file))
write_csv(combined_data, output_file)

cat(sprintf("Successfully created combined dataset with %d rows and %d columns\n", 
            nrow(combined_data), ncol(combined_data)))
cat(sprintf("Output file: %s\n", output_file))

# Print summary
cat("\nSummary:\n")
cat(sprintf("Total CGM-only patients: %d\n", nrow(combined_data)))
cat(sprintf("Patients with demographics: %d\n", sum(!is.na(combined_data$AgeAtEnrollment))))
cat(sprintf("Patients with CGM data: %d\n", sum(!is.na(combined_data$n_cgm_readings))))

