# ============================================
# create_demographics_table.R
# Purpose: Combine demographic information from multiple source files
# ============================================

library(dplyr)
library(readr)

# Define file paths
data_dir <- "data/raw/Data Files"
output_dir <- "output/tables"

# Read all source files
cat("Reading source files...\n")

# 1. Screening data (main demographic info)
screening <- read_delim(
  file.path(data_dir, "DiabScreening_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  select(PtID, AgeAtEnrollment, Gender, Ethnicity, Race, DiagAge, DiagAgeApprox) %>%
  distinct(PtID, .keep_all = TRUE)

# 2. Physical exam data (weight, height, blood pressure)
phys_exam <- read_delim(
  file.path(data_dir, "DiabPhysExam_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  filter(Visit == "Screening" | is.na(Visit) | Visit == "") %>%
  select(PtID, Weight, WeightUnits, Height, HeightUnits, BldPrSys, BldPrDia) %>%
  distinct(PtID, .keep_all = TRUE) %>%
  # Convert weight to kg if needed
  mutate(
    Weight_kg = case_when(
      tolower(WeightUnits) %in% c("kg", "kgs") ~ as.numeric(Weight),
      tolower(WeightUnits) %in% c("lbs", "lb") ~ as.numeric(Weight) * 0.453592,
      TRUE ~ NA_real_
    ),
    # Convert height to cm if needed
    Height_cm = case_when(
      tolower(HeightUnits) == "cm" ~ as.numeric(Height),
      tolower(HeightUnits) %in% c("in", "inch") ~ as.numeric(Height) * 2.54,
      TRUE ~ NA_real_
    ),
    # Calculate BMI
    BMI = Weight_kg / ((Height_cm / 100)^2)
  ) %>%
  select(PtID, Weight_kg, Height_cm, BMI, BldPrSys, BldPrDia)

# 3. Socioeconomic data
socio_econ <- read_delim(
  file.path(data_dir, "DiabSocioEcon_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  select(PtID, EducationLevel, AnnualIncome, 
         InsPrivate, InsMedicare, InsMedicaid, InsSCHIP, InsMilitary, 
         InsOtherGov, InsNoCoverage, InsUnk) %>%
  distinct(PtID, .keep_all = TRUE) %>%
  # Create insurance type summary
  mutate(
    InsuranceType = case_when(
      InsPrivate == "1" ~ "Private",
      InsMedicare == "1" ~ "Medicare",
      InsMedicaid == "1" ~ "Medicaid",
      InsSCHIP == "1" ~ "SCHIP",
      InsMilitary == "1" ~ "Military",
      InsOtherGov == "1" ~ "Other Government",
      InsNoCoverage == "1" ~ "No Coverage",
      InsUnk == "1" ~ "Unknown",
      TRUE ~ "Not Specified"
    )
  ) %>%
  select(PtID, EducationLevel, AnnualIncome, InsuranceType)

# 4. HbA1c data (screening visit)
hba1c <- read_delim(
  file.path(data_dir, "DiabLocalHbA1c_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  filter(Visit == "Screening") %>%
  select(PtID, HbA1cTestRes, HbA1cTestDt) %>%
  distinct(PtID, .keep_all = TRUE) %>%
  mutate(HbA1c_Screening = as.numeric(HbA1cTestRes)) %>%
  select(PtID, HbA1c_Screening, HbA1cTestDt)

# 5. Medical conditions (count and list major conditions)
med_conditions <- read_delim(
  file.path(data_dir, "MedicalCondition_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  filter(MedCondPreStart == "Yes") %>%
  select(PtID, MedCondPreStartCat) %>%
  group_by(PtID) %>%
  summarise(
    NumMedicalConditions = n(),
    MedicalConditions = paste(unique(MedCondPreStartCat), collapse = "; ")
  )

# 6. Medications (count and categorize)
medications <- read_delim(
  file.path(data_dir, "Medication_a.txt"),
  delim = "|",
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) %>%
  filter(MedOngoing == "On treatment at time of enrollment" | 
         is.na(MedOngoing) | MedOngoing == "") %>%
  select(PtID, ParentRxNormDrugListID) %>%
  filter(!is.na(ParentRxNormDrugListID) & ParentRxNormDrugListID != "") %>%
  group_by(PtID) %>%
  summarise(
    NumMedications = n(),
    Medications = paste(unique(ParentRxNormDrugListID), collapse = "; ")
  )

# Combine all data
cat("Combining demographic data...\n")

demographics <- screening %>%
  left_join(phys_exam, by = "PtID") %>%
  left_join(socio_econ, by = "PtID") %>%
  left_join(hba1c, by = "PtID") %>%
  left_join(med_conditions, by = "PtID") %>%
  left_join(medications, by = "PtID") %>%
  # Replace NA counts with 0
  mutate(
    NumMedicalConditions = if_else(is.na(NumMedicalConditions), 0, NumMedicalConditions),
    NumMedications = if_else(is.na(NumMedications), 0, NumMedications),
    MedicalConditions = if_else(is.na(MedicalConditions), "", MedicalConditions),
    Medications = if_else(is.na(Medications), "", Medications)
  ) %>%
  # Reorder columns for better readability
  select(
    PtID,
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
  ) %>%
  arrange(PtID)

# Write to CSV
output_file <- file.path(output_dir, "patient_demographics.csv")
write_csv(demographics, output_file)

cat(sprintf("Demographics table created: %s\n", output_file))
cat(sprintf("Total patients: %d\n", nrow(demographics)))
cat(sprintf("Total columns: %d\n", ncol(demographics)))

# Print summary
cat("\nSummary statistics:\n")
cat(sprintf("Patients with age: %d\n", sum(!is.na(demographics$AgeAtEnrollment))))
cat(sprintf("Patients with gender: %d\n", sum(!is.na(demographics$Gender))))
cat(sprintf("Patients with BMI: %d\n", sum(!is.na(demographics$BMI))))
cat(sprintf("Patients with HbA1c: %d\n", sum(!is.na(demographics$HbA1c_Screening))))
cat(sprintf("Patients with education: %d\n", sum(!is.na(demographics$EducationLevel))))
cat(sprintf("Patients with income: %d\n", sum(!is.na(demographics$AnnualIncome))))

