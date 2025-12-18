# DCLP3 Glycemic Dashboard (Group Stats — Final Project)
**Connor Moore • Eimee Co • Jin-sung Sur**

This repository contains a reproducible, end-to-end workflow in **R** to (1) transform raw DCLP3 trial text files into analytic patient-level features, (2) engineer insulin delivery features such as **Total Daily Dose (TDD)**, (3) train and evaluate classification models for glycemic control (**AUC + ROC**), and (4) publish results as a multi-page **Quarto website** deployed via **GitHub Pages** (`docs/`).

> **Important (Data Governance):** Raw DCLP3 files are **not included** in this repository and should not be committed. This repo is designed to be reproducible with local raw data access (via an environment variable).

---

## Table of Contents
- [1. Project Overview](#1-project-overview)
- [2. Clinical Definitions and Outcomes](#2-clinical-definitions-and-outcomes)
- [3. Quick Start](#3-quick-start)
- [4. Repository Structure](#4-repository-structure)
- [5. Data Inputs](#5-data-inputs)
- [6. Reproducibility Infrastructure](#6-reproducibility-infrastructure)
- [7. Pipeline Execution](#7-pipeline-execution)
- [8. Script-by-Script Documentation](#8-script-by-script-documentation)
- [9. Output Artifacts](#9-output-artifacts)
- [10. Modeling and Evaluation](#10-modeling-and-evaluation)
- [11. CGM-only Cohort and Baseline Transfer](#11-cgm-only-cohort-and-baseline-transfer)
- [12. Quarto Website Product](#12-quarto-website-product)
- [13. Deployment via GitHub Pages](#13-deployment-via-github-pages)
- [14. QA Checks and Troubleshooting](#14-qa-checks-and-troubleshooting)
- [15. Paradigm Integration (Course Alignment)](#15-paradigm-integration-course-alignment)
- [16. Limitations and Next Steps](#16-limitations-and-next-steps)
- [17. Credits](#17-credits)

---

## 1. Project Overview

### 1.1 What problem are we solving?
We focus on **glycemic control** measured by **Time-in-Range (TIR)**, defined as the percent of CGM time a patient’s glucose is between **70–180 mg/dL**. A widely used clinical target is **TIR ≥ 70%**. Patients below this target are candidates for intensified clinical attention and diabetes technology support.

### 1.2 What this repository delivers
1. A reproducible **pipeline** that ingests raw DCLP3 text files, cleans/joins them by patient, and builds analytic tables.
2. Patient-level feature engineering for:
   - **baseline demographics and HbA1c**
   - CGM summaries (TIR and variability)
   - pump insulin delivery summaries (basal, bolus)
   - algorithmic derivation of **daily TDD**
3. Two modeling variants:
   - **No-insulin baseline** model (transferable to CGM-only patients)
   - **Full** model (adds insulin delivery features)
4. A multi-page **Quarto website** with:
   - cohort overview
   - patient-level explorer (daily TDD + summary table)
   - model metrics and ROC curves
   - browsable output tables

---

## 2. Clinical Definitions and Outcomes

### 2.1 Time-in-Range (TIR)
- **TIR 70–180** = percent of CGM readings between 70 and 180 mg/dL.
- TIR is clinically interpretable and often used to summarize glucose quality and risk.

### 2.2 Binary outcome for modeling
We define a binary classification target:

- **Uncontrolled = 1** if `pct_70_180 < 70`
- **Uncontrolled = 0** if `pct_70_180 >= 70`

This aligns with the dashboard’s emphasis on the **70% target threshold**.

---

## 3. Quick Start

### 3.1 Prerequisites
- R (RStudio IDE recommended)
- Quarto CLI installed (`quarto` available on PATH)
- R packages listed below

### 3.2 Install R packages
From an R session:
```r
install.packages(c(
  "here","readr","dplyr","lubridate","ggplot2",
  "DT","plotly","crosstalk","stringr",
  "caret","glmnet","pROC","iglu",
  "purrr","tibble"
))
```

### 3.3 Set raw data location
Default expected path:
- `data/raw/Data Files/`

If raw files are elsewhere, set `DCLP3_RAW_DIR`:

**In R:**
```r
Sys.setenv(DCLP3_RAW_DIR = "C:/path/to/Data Files")
```

**In Git Bash:**
```bash
export DCLP3_RAW_DIR="/c/path/to/Data Files"
```

### 3.4 Run the pipeline
From repo root:
```bash
Rscript run_pipeline.R
```

Force rebuild all steps:
```bash
FORCE_REBUILD=TRUE Rscript run_pipeline.R
```

### 3.5 Render the Quarto website
```bash
quarto render
```
This writes the built site to `docs/`.

---

## 4. Repository Structure
```text
.
├─ src/                          # pipeline scripts (modular)
│  ├─ _paths.R                   # central paths + RAW_DIR override
│  ├─ utils/
│  │  └─ helper_functions.R      # reusable feature + iglu helpers
│  ├─ create_demographics_table.R
│  ├─ summarize_pump_data_by_patient.R
│  ├─ summarize_cgm_by_patient.R
│  ├─ calculate_tdd.R
│  ├─ combine_patient_data.R
│  ├─ identifypt.R
│  ├─ PumpsettingsID.R
│  ├─ combine_cgm_only_patient_data.R
│  ├─ ml_auc_models.R
│  └─ predict_cgm_only_patients.R
├─ output/
│  ├─ tables/                    # derived tables (CSV)
│  ├─ figures/                   # figures (PNG) including ROC curves
│  └─ models/                    # serialized model artifacts (.rds)
├─ data/
│  ├─ raw/                       # raw DCLP3 files (NOT tracked)
│  └─ processed/                 # intermediate cohort artifacts (CGM-only PtIDs)
├─ index.qmd / overview.qmd / models.qmd / tables.qmd / patient-explorer.qmd
├─ _quarto.yml                   # Quarto site config (renders into docs/)
├─ styles.css                    # site styling
└─ docs/                         # rendered site for GitHub Pages
```

---

## 5. Data Inputs

### 5.1 Raw files used
Baseline / screening:
- `DiabScreening_a.txt`
- `DiabPhysExam_a.txt`
- `DiabSocioEcon_a.txt`
- `DiabLocalHbA1c_a.txt`
- `MedicalCondition_a.txt`
- `Medication_a.txt`

Pump/CGM:
- `Pump_BasalRateChange.txt`
- `Pump_BolusDelivered.txt`
- `Pump_CGMGlucoseValue.txt`
- `InsulinPumpSettings_a.txt`

CGM-only:
- `cgm.txt`

### 5.2 File format caveats
- Several baseline files are **UTF-16LE** and pipe-delimited (`|`).
- Pump logs are typically **UTF-8** and may require robust parsing.
- Datetime formats vary; scripts use safe parsing to reduce failures.

---

## 6. Reproducibility Infrastructure

### 6.1 Central paths: `src/_paths.R`
**Purpose:** make scripts portable and consistent by centralizing:
- where raw data lives (`RAW_DIR`, with `DCLP3_RAW_DIR` override),
- where outputs are written (`output/tables`, `output/figures`, `output/models`),
- helper path constructors (`raw()`, `tbl()`, `fig()`, `mdl()`),
- guardrails (`assert_exists()`, `assert_raw_files()`).

This eliminates hard-coded absolute paths and prevents “works on my machine” failures.

### 6.2 Orchestrator: `run_pipeline.R`
**Purpose:** provide a single entry point that:
- runs steps in dependency order,
- prints configuration (root + key directories),
- skips steps when outputs already exist (unless `FORCE_REBUILD=TRUE`),
- fails fast on errors so the pipeline does not continue with partial outputs.

---

## 7. Pipeline Execution

### 7.1 Pipeline concept
The pipeline is a dependency chain:
- raw files → patient-level summaries → combined analytic tables → models/figures → website

### 7.2 Step order (as executed by `run_pipeline.R`)
1. Create demographics table
2. Summarize pump data by patient (basal/bolus/pump CGM)
3. Summarize `cgm.txt` by patient
4. Calculate TDD (daily + summary + QC figures)
5. Combine pump cohort table (`combined_patient_data.csv`)
6. Identify CGM-only PtIDs (`cgm_only_ptids.csv`)
7. Sanity check pump settings vs CGM-only (counts table)
8. Combine CGM-only cohort table (`combined_cgm_only_patient_data.csv`)
9. Train pump-cohort models (AUC + ROC + artifacts)
10. Predict CGM-only cohort using baseline transfer model

---

## 8. Script-by-Script Documentation

### 8.1 `src/create_demographics_table.R`
**Objective:** build `patient_demographics.csv` with one row per patient:
- screening demographics (age/sex/race/ethnicity/diagnosis age)
- physical exam (weight/height → BMI, BP)
- socioeconomic (education/income + InsuranceType)
- HbA1c at screening
- counts and lists of conditions and medications

**Key decisions:**
- **screening visit** used as baseline
- units standardized to kg/cm for BMI computation
- insurance indicators collapsed to a single label

**Output:**
- `output/tables/patient_demographics.csv`

### 8.2 `src/summarize_pump_data_by_patient.R`
**Objective:** summarize pump logs into patient-level tables:
- basal rate change summary (mean/SD/IQR/CV, days observed)
- bolus delivery summary (totals, frequency/day, variability, type breakdown)
- pump CGM summary (mean/SD/CV and TIR-like proportions)

**Outputs:**
- `output/tables/basal_rate_summary_by_patient.csv`
- `output/tables/bolus_delivery_summary_by_patient.csv`
- `output/tables/cgm_summary_by_patient.csv`
- `output/tables/pump_data_combined_summary_by_patient.csv`

### 8.3 `src/summarize_cgm_by_patient.R` (cgm.txt)
**Objective:** summarize `cgm.txt` into patient-level CGM metrics:
- glucose mean/median/SD/CV/IQR
- TIR-like proportions (<70, 70–180, >180, >250)
- date coverage information
- optional iglu metrics (when computable)

**Output:**
- `output/tables/cgm_txt_summary_by_patient.csv`

### 8.4 `src/calculate_tdd.R`
**Objective:** compute insulin delivery features:
- daily basal insulin via integration of rate over time
- daily bolus insulin via sum of events
- daily TDD = basal + bolus
- per-patient summaries (mean, SD, CV; basal_pct/bolus_pct; boluses/day)
- QC figures (distributions and trends)

**Outputs (tables):**
- `output/tables/tdd_summary_by_patient.csv`
- `output/tables/tdd_overall_summary.csv`
- (required by Patient Explorer) `output/tables/daily_tdd_by_patient.csv` (PtID, Date, tdd)

**Outputs (figures):**
- `output/figures/tdd_distribution.png`
- `output/figures/tdd_by_patient_boxplot.png`
- `output/figures/basal_vs_bolus_scatter.png`
- `output/figures/tdd_over_time_summarized.png`
- `output/figures/tdd_over_time_weekly.png`

### 8.5 `src/combine_patient_data.R`
**Objective:** create `combined_patient_data.csv` (pump cohort) by joining:
- TDD summaries
- demographics
- pump CGM summaries

Adds derived features:
- `TDD_per_kg = tdd_mean / Weight_kg`
- `TDD_per_BMI = tdd_mean / BMI`

**Output:**
- `output/tables/combined_patient_data.csv`

### 8.6 `src/identifypt.R`
**Objective:** define CGM-only cohort:
- PtIDs in `cgm.txt` that do not appear in pump basal/bolus logs.

**Outputs:**
- `data/processed/ptid_counts.csv`
- `data/processed/cgm_only_ptids.csv`

### 8.7 `src/PumpsettingsID.R`
**Objective:** QA check: compare CGM-only PtIDs to pump settings PtIDs.

**Output:**
- `output/tables/pumpsettings_vs_cgm_only_counts.csv`

### 8.8 `src/combine_cgm_only_patient_data.R`
**Objective:** build CGM-only analytic table by joining:
- `cgm_txt_summary_by_patient.csv`
- `patient_demographics.csv`
filtered to CGM-only PtIDs.

**Output:**
- `output/tables/combined_cgm_only_patient_data.csv`

### 8.9 `src/ml_auc_models.R`
**Objective:** train two pump-cohort models:
- **No-insulin baseline** (transferable)
- **Full model** (adds insulin features)

Workflow includes:
- outcome definition from TIR
- stratified train/test split
- train-only imputation
- robust categorical handling
- LASSO logistic regression via `cv.glmnet`
- ROC/AUC evaluation and ROC plot export
- saving artifacts for transfer prediction

**Outputs:**
- `output/tables/ml_metrics_pump.csv`
- `output/tables/ml_selected_vars_full.csv`
- `output/tables/ml_selected_vars_no_insulin.csv`
- `output/figures/roc_full.png`
- `output/figures/roc_no_insulin.png`
- `output/models/model_artifacts_full.rds`
- `output/models/model_artifacts_no_insulin.rds`
- `output/models/baseline_no_insulin_model.rds`

### 8.10 `src/predict_cgm_only_patients.R`
**Objective:** apply the no-insulin baseline model to the CGM-only cohort:
- aligns CGM-only design matrix to training columns
- uses training medians where available
- safe factor handling to prevent contrasts errors
- exports predictions and metrics
- optional ROC if AUC computable

**Outputs:**
- `output/tables/cgm_only_predictions.csv`
- `output/tables/cgm_only_performance_metrics.csv`
- optional `output/figures/roc_cgm_only.png`

---

## 9. Output Artifacts

### 9.1 Tables (`output/tables/`)
Core deliverables include:
- `patient_demographics.csv`
- `basal_rate_summary_by_patient.csv`
- `bolus_delivery_summary_by_patient.csv`
- `cgm_summary_by_patient.csv`
- `cgm_txt_summary_by_patient.csv`
- `tdd_summary_by_patient.csv`
- `tdd_overall_summary.csv`
- `daily_tdd_by_patient.csv`
- `combined_patient_data.csv`
- `combined_cgm_only_patient_data.csv`
- `ml_metrics_pump.csv`
- `ml_selected_vars_full.csv`
- `ml_selected_vars_no_insulin.csv`
- `cgm_only_predictions.csv`
- `cgm_only_performance_metrics.csv`
- `pumpsettings_vs_cgm_only_counts.csv`

### 9.2 Figures (`output/figures/`)
- TDD QC plots (distribution, trends)
- `roc_no_insulin.png`
- `roc_full.png`
- `roc_cgm_only.png`

### 9.3 Models (`output/models/`)
- `model_artifacts_full.rds`
- `model_artifacts_no_insulin.rds`
- `baseline_no_insulin_model.rds`

---

## 10. Modeling and Evaluation

### 10.1 Why two models?
- The **full** model tests whether insulin delivery features improve prediction.
- The **no-insulin baseline** enables scoring on patients without pump logs (CGM-only cohort).

### 10.2 Why LASSO logistic regression?
- Predictors are correlated and sample size is modest.
- LASSO provides regularization and variable selection.

### 10.3 Why ROC/AUC in addition to accuracy?
- Accuracy depends on a single threshold (often 0.5).
- AUC summarizes discrimination across thresholds and is more robust for comparing models.

### 10.4 Leakage prevention
- imputation is learned on training data only
- factors are handled to avoid test-only levels
- test design matrix is aligned to training columns

---

## 11. CGM-only Cohort and Baseline Transfer

### 11.1 Cohort rationale
Some patients appear in `cgm.txt` but not in pump basal/bolus logs. These patients lack insulin delivery features but still support CGM-based features and demographics.

### 11.2 Transfer approach
We apply the **no-insulin baseline** model artifact to CGM-only patients and export:
- predicted probability of uncontrolled
- predicted class
- evaluation metrics when true outcome is available

---

## 12. Quarto Website Product

### 12.1 Site configuration (`_quarto.yml`)
- builds to `docs/` for GitHub Pages
- renders only main pages
- copies `styles.css` and `output/figures/**` into `docs/`
- defines navigation

### 12.2 Pages
- `index.qmd`: site purpose + regeneration steps
- `overview.qmd`: cohort snapshot + TIR distribution + HbA1c vs TIR
- `patient-explorer.qmd`: daily TDD plot + patient summary table + patient selector
- `models.qmd`: model metrics + ROC images
- `tables.qmd`: searchable DataTables with readable column names

### 12.3 Styling (`styles.css`)
- consistent panel styling
- responsive images
- DataTables formatting (scrolling, no-wrap)
- enlarged ROC display

---

## 13. Deployment via GitHub Pages

1) Render:
```bash
quarto render
```

2) Commit and push:
```bash
git add docs
git commit -m "Render site"
git push
```

3) GitHub Settings → Pages:
- Source: Deploy from branch
- Branch: `main`
- Folder: `/docs`

---

## 14. QA Checks and Troubleshooting

### 14.1 “Missing file” during render
Run pipeline first, then render:
```bash
Rscript run_pipeline.R
quarto render
```

### 14.2 Encoding errors (baseline files)
If reads fail, verify UTF-16LE settings on Diab* / Medication* / MedicalCondition* inputs.

### 14.3 Patient Explorer appears empty
Confirm:
- `output/tables/daily_tdd_by_patient.csv` exists
- Date parses correctly
- date window overlaps available data

### 14.4 Variable selection differs across runs
LASSO selection can vary with splits and correlated predictors; keep a fixed seed for stable reporting.

---

## 15. Paradigm Integration (Course Alignment)
This project integrates multiple paradigms:
- **Command line:** `Rscript run_pipeline.R`, `quarto render`, Git/GitHub deployment workflow
- **Functional modularity:** reusable helpers (`_paths.R`, `helper_functions.R`), robust readers/parsers
- **Machine learning workflow:** train/test separation, LASSO logistic regression, ROC/AUC evaluation
- **Product:** deployed multi-page Quarto dashboard (`docs/`)

---

## 16. Limitations and Next Steps
Not yet implemented (planned improvements):
- calibration plots (reliability curves / Brier score)
- `testthat` unit tests for core helper functions and key transformations
- more models (e.g., tree-based) and external validation

---

## 17. Credits
Made by **The Three Mouseketeers**:
- Connor Moore
- Eimee Co
- Jin-sung Sur
