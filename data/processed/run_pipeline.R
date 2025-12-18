# ============================================
# run_pipeline.R
# Reproducible pipeline runner
# Run: Rscript run_pipeline.R
# Optional: Sys.setenv(FORCE_REBUILD="TRUE")
# ============================================

suppressPackageStartupMessages(library(here))
source(here("src", "_paths.R"))
ensure_dirs()

FORCE_REBUILD <- identical(toupper(Sys.getenv("FORCE_REBUILD")), "TRUE")

cat("============================================================\n")
cat("group-stats pipeline\n")
cat("Project root:", here::here(), "\n")
cat("RAW_DIR:     ", RAW_DIR, "\n")
cat("TABLES_DIR:  ", TABLES_DIR, "\n")
cat("FIG_DIR:     ", FIG_DIR, "\n")
cat("MODELS_DIR:  ", MODELS_DIR, "\n")
cat("FORCE_REBUILD:", FORCE_REBUILD, "\n")
cat("============================================================\n\n")

step_done <- function(outputs) {
  length(outputs) > 0 && all(file.exists(outputs))
}

run_step <- function(label, script_rel_path, outputs = character(0)) {
  cat("------------------------------------------------------------\n")
  cat("STEP:   ", label, "\n")
  cat("SCRIPT: ", script_rel_path, "\n")
  cat("------------------------------------------------------------\n")
  
  if (!FORCE_REBUILD && step_done(outputs)) {
    cat("↪ Skipping (outputs already exist)\n\n")
    return(invisible(TRUE))
  }
  
  script_path <- here::here(script_rel_path)
  if (!file.exists(script_path)) stop("Script not found: ", script_path, call. = FALSE)
  
  tryCatch(
    {
      source(script_path, local = TRUE)
      cat("✔ Completed: ", label, "\n\n")
    },
    error = function(e) {
      cat("✖ ERROR in step: ", label, "\n")
      cat("  ", conditionMessage(e), "\n")
      stop(e)
    }
  )
}

# ---- Pipeline ----

run_step(
  "Create demographics table",
  file.path("src", "create_demographics_table.R"),
  outputs = c(tbl("patient_demographics.csv"))
)

run_step(
  "Summarize pump data by patient (basal/bolus/pump CGM)",
  file.path("src", "summarize_pump_data_by_patient.R"),
  outputs = c(
    tbl("basal_rate_summary_by_patient.csv"),
    tbl("bolus_delivery_summary_by_patient.csv"),
    tbl("cgm_summary_by_patient.csv"),
    tbl("pump_data_combined_summary_by_patient.csv")
  )
)

run_step(
  "Summarize cgm.txt by patient",
  file.path("src", "summarize_cgm_by_patient.R"),
  outputs = c(tbl("cgm_txt_summary_by_patient.csv"))
)

run_step(
  "Calculate TDD",
  file.path("src", "calculate_tdd.R"),
  outputs = c(tbl("tdd_summary_by_patient.csv"), tbl("tdd_overall_summary.csv"))
)

run_step(
  "Combine pump cohort table",
  file.path("src", "combine_patient_data.R"),
  outputs = c(tbl("combined_patient_data.csv"))
)

run_step(
  "Identify CGM-only patients",
  file.path("src", "identifypt.R"),
  outputs = c(tbl("cgm_only_ptids.csv"), tbl("ptid_counts.csv"))
)

run_step(
  "Sanity check pump settings vs CGM-only",
  file.path("src", "PumpsettingsID.R"),
  outputs = c(tbl("pumpsettings_vs_cgm_only_counts.csv"))
)

run_step(
  "Combine CGM-only cohort table",
  file.path("src", "combine_cgm_only_patient_data.R"),
  outputs = c(tbl("combined_cgm_only_patient_data.csv"))
)

run_step(
  "ML AUC models (pump cohort)",
  file.path("src", "ml_auc_models.R"),
  outputs = c(tbl("ml_metrics_pump.csv"), mdl("baseline_no_insulin_model.rds"))
)

run_step(
  "Predict CGM-only patients",
  file.path("src", "predict_cgm_only_patients.R"),
  outputs = c(tbl("cgm_only_predictions.csv"), tbl("cgm_only_performance_metrics.csv"))
)

cat("============================================================\n")
cat("Pipeline complete ✅\n")
cat("============================================================\n")
