# ============================================
# ml_auc_models.R
# Purpose:
#   - Read combined_patient_data.csv (pump cohort)
#   - Create outcome: Uncontrolled = 1 if pct_70_180 < 70, else 0
#   - Build:
#       (A) Full model (includes insulin-related predictors)
#       (B) No-insulin model (excludes insulin-related predictors)
#   - Fit lasso on TRAIN only to select predictors
#   - Fit interpretable glm on selected predictors
#   - Evaluate on TEST: accuracy + AUC, save ROC plots + model artifacts
#
# Inputs:
#   output/tables/combined_patient_data.csv
#
# Outputs:
#   output/tables/ml_metrics_pump.csv
#   output/tables/ml_selected_vars_full.csv
#   output/tables/ml_selected_vars_no_insulin.csv
#   output/figures/roc_full.png
#   output/figures/roc_no_insulin.png
#   output/models/glm_full_selected.rds
#   output/models/glm_no_insulin_selected.rds   (also used for CGM-only transfer)
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(caret)
  library(glmnet)
  library(pROC)
})

in_file    <- here("output", "tables", "combined_patient_data.csv")
out_tables <- here("output", "tables")
out_figs   <- here("output", "figures")
out_models <- here("output", "models")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figs, recursive = TRUE, showWarnings = FALSE)
dir.create(out_models, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) stop("Missing input: ", in_file)

set.seed(1)

df <- read_csv(in_file, show_col_types = FALSE)

if (!("pct_70_180" %in% names(df))) stop("combined_patient_data.csv must contain pct_70_180")

# Outcome: uncontrolled if TIR < 70%
df <- df %>%
  mutate(Uncontrolled = if_else(is.na(pct_70_180), NA_integer_, if_else(pct_70_180 < 70, 1L, 0L)))

# Candidate predictors (from your ML AUC doc)
predictor_vars <- c(
  "AgeAtEnrollment","Gender","Ethnicity","Race","DiagAge","DiagAgeApprox",
  "Weight_kg","Height_cm","BMI","BldPrSys","BldPrDia","EducationLevel","AnnualIncome","InsuranceType",
  "HbA1c_Screening","NumMedicalConditions","NumMedications",
  "tdd_mean","tdd_median","tdd_sd","tdd_cv","TDD_per_kg","TDD_per_BMI",
  "basal_mean","basal_median","basal_sd","basal_pct",
  "bolus_mean","bolus_median","bolus_sd","bolus_pct","mean_boluses_per_day",
  "n_days"
)

insulin_vars <- c(
  "tdd_mean","tdd_median","tdd_sd","tdd_cv","TDD_per_kg","TDD_per_BMI",
  "basal_mean","basal_median","basal_sd","basal_pct",
  "bolus_mean","bolus_median","bolus_sd","bolus_pct",
  "mean_boluses_per_day"
)

present_predictors <- intersect(predictor_vars, names(df))
if (length(setdiff(predictor_vars, present_predictors)) > 0) {
  warning("These predictors were not found in combined_patient_data.csv and will be skipped: ",
          paste(setdiff(predictor_vars, present_predictors), collapse = ", "))
}

ModelDF_full <- df %>%
  select(all_of(present_predictors), Uncontrolled) %>%
  filter(!is.na(Uncontrolled))

ModelDF_no_insulin <- df %>%
  select(all_of(setdiff(present_predictors, insulin_vars)), Uncontrolled) %>%
  filter(!is.na(Uncontrolled))

prep_split <- function(dat) {
  idx <- createDataPartition(dat$Uncontrolled, p = 0.8, list = FALSE)
  train <- dat[idx, , drop = FALSE]
  test  <- dat[-idx, , drop = FALSE]
  list(train = train, test = test)
}

fit_lasso_then_glm <- function(dat, label) {
  split <- prep_split(dat)
  train <- split$train
  test  <- split$test
  
  x_train <- train %>% select(-Uncontrolled)
  y_train <- as.integer(train$Uncontrolled)
  
  x_test  <- test %>% select(-Uncontrolled)
  y_test  <- as.integer(test$Uncontrolled)
  
  # Impute numeric predictors using TRAIN medians only
  preProc <- preProcess(x_train, method = "medianImpute")
  x_train_imp <- predict(preProc, x_train)
  x_test_imp  <- predict(preProc, x_test)
  
  # Drop predictors that are ALL NA in training (median impute can't fix those)
  all_na_cols <- names(x_train_imp)[vapply(x_train_imp, function(z) all(is.na(z)), logical(1))]
  if (length(all_na_cols) > 0) {
    message("Dropping all-NA predictors (train): ", paste(all_na_cols, collapse = ", "))
    x_train_imp <- x_train_imp %>% select(-all_of(all_na_cols))
    x_test_imp  <- x_test_imp  %>% select(-all_of(all_na_cols))
  }
  
  # Character -> factor, with explicit handling of unseen test levels
  char_cols <- names(x_train_imp)[vapply(x_train_imp, is.character, logical(1))]
  for (col in char_cols) {
    tr <- as.character(x_train_imp[[col]])
    te <- as.character(x_test_imp[[col]])
    
    tr[is.na(tr) | tr == ""] <- "Unknown"
    te[is.na(te) | te == ""] <- "Unknown"
    
    # Ensure train levels include Unknown
    lv <- unique(tr)
    if (!("Unknown" %in% lv)) lv <- c(lv, "Unknown")
    
    # Any test value not seen in training -> Unknown (prevents NA)
    te[!te %in% lv] <- "Unknown"
    
    x_train_imp[[col]] <- factor(tr, levels = lv)
    x_test_imp[[col]]  <- factor(te, levels = lv)
  }
  
  train_imp <- bind_cols(x_train_imp, Uncontrolled = y_train)
  test_imp  <- bind_cols(x_test_imp,  Uncontrolled = y_test)
  
  # ---- Build TRAIN matrix (do NOT drop rows silently) ----
  x_mat <- model.matrix(Uncontrolled ~ ., data = train_imp, na.action = na.pass)
  x_mat <- x_mat[, -1, drop = FALSE]
  
  keep_train <- complete.cases(x_mat)
  if (!all(keep_train)) {
    message("Dropping ", sum(!keep_train), " TRAIN rows due to remaining NA in model matrix (", label, ").")
    x_mat   <- x_mat[keep_train, , drop = FALSE]
    y_train <- y_train[keep_train]
  }
  
  # Fit lasso (TRAIN only)
  cv_lasso <- cv.glmnet(x_mat, y_train, alpha = 1, family = "binomial")
  
  coef_min <- coef(cv_lasso, s = "lambda.min")
  nz <- rownames(coef_min)[as.vector(coef_min) != 0]
  nz <- setdiff(nz, "(Intercept)")
  
  write_csv(
    tibble(selected_term = nz),
    file.path(out_tables, paste0("ml_selected_vars_", label, ".csv"))
  )
  
  # ---- Build TEST matrix (do NOT drop rows silently) ----
  x_test_mat <- model.matrix(Uncontrolled ~ ., data = test_imp, na.action = na.pass)
  x_test_mat <- x_test_mat[, -1, drop = FALSE]
  
  keep_test <- complete.cases(x_test_mat)
  if (!all(keep_test)) {
    message("Dropping ", sum(!keep_test), " TEST rows due to remaining NA in model matrix (", label, ").")
    x_test_mat <- x_test_mat[keep_test, , drop = FALSE]
    y_test     <- y_test[keep_test]
  }
  
  # Align columns (dummy columns can differ if factors differ)
  train_cols <- colnames(x_mat)
  
  # Keep cols seen in training
  x_test_mat <- x_test_mat[, intersect(train_cols, colnames(x_test_mat)), drop = FALSE]
  
  # Add missing training cols as zeros
  missing_cols <- setdiff(train_cols, colnames(x_test_mat))
  if (length(missing_cols) > 0) {
    zeros <- matrix(0, nrow = nrow(x_test_mat), ncol = length(missing_cols),
                    dimnames = list(NULL, missing_cols))
    x_test_mat <- cbind(x_test_mat, zeros)
  }
  
  # Reorder to match training
  x_test_mat <- x_test_mat[, train_cols, drop = FALSE]
  
  prob <- predict(cv_lasso, newx = x_test_mat, s = "lambda.min", type = "response")
  prob <- as.numeric(prob)
  
  # Now these MUST match
  if (length(prob) != length(y_test)) {
    stop(sprintf("Internal length mismatch after NA-handling: prob=%d, y_test=%d (label=%s).",
                 length(prob), length(y_test), label), call. = FALSE)
  }
  
  pred <- as.integer(prob > 0.5)
  acc  <- mean(pred == y_test) * 100
  
  # ROC/AUC guard: need both classes in test
  auc_val <- NA_real_
  if (length(unique(y_test)) >= 2) {
    roc_obj <- roc(response = y_test, predictor = prob, quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    
    pretty_label <- switch(label,
                           "full" = "ROC curve — Full model (pump cohort, test set)",
                           "no_insulin" = "ROC curve — No-insulin baseline (pump cohort, test set)",
                           paste0("ROC curve — ", label)
    )
    
    col_line <- switch(label,
                       "full" = "#1f77b4",
                       "no_insulin" = "#ff7f0e",
                       "#1f77b4"
    )
    
    png(file.path(out_figs, paste0("roc_", label, ".png")), width = 1100, height = 800)
    par(mar = c(4.5, 4.5, 3, 1))
    plot(
      roc_obj,
      col = col_line,
      lwd = 4,
      legacy.axes = TRUE,
      main = pretty_label,
      xlab = "False positive rate (1 - specificity)",
      ylab = "True positive rate (sensitivity)"
    )
    grid(col = "#d1d5db")
    text(0.62, 0.18, labels = paste0("AUC = ", round(auc_val, 3)), cex = 1.2)
    dev.off()
  } else {
    warning("ROC/AUC skipped: test set has only one class for label=", label)
  }
  
  # Save artifacts needed for transfer
  saveRDS(
    list(
      label = label,
      selected_terms = nz,
      cv_lasso = cv_lasso,
      preProc = preProc,
      threshold = 0.5,
      train_cols = train_cols,
      outcome_rule = "Uncontrolled = 1 if pct_70_180 < 70"
    ),
    file.path(out_models, paste0("model_artifacts_", label, ".rds"))
  )
  
  tibble(
    model = label,
    n_train = nrow(x_mat),
    n_test  = nrow(x_test_mat),
    accuracy_pct = acc,
    auc = auc_val,
    used_glm = FALSE
  )
}


cat("Running ML + AUC on pump cohort...\n")

metrics_full      <- fit_lasso_then_glm(ModelDF_full, "full")
metrics_no_insulin <- fit_lasso_then_glm(ModelDF_no_insulin, "no_insulin")

metrics <- bind_rows(metrics_full, metrics_no_insulin)
write_csv(metrics, file.path(out_tables, "ml_metrics_pump.csv"))

# Convenience: also save the no-insulin artifact under a stable name for downstream scripts
file.copy(
  file.path(out_models, "model_artifacts_no_insulin.rds"),
  file.path(out_models, "baseline_no_insulin_model.rds"),
  overwrite = TRUE
)

cat("Saved:\n")
cat("  - ", file.path(out_tables, "ml_metrics_pump.csv"), "\n", sep = "")
cat("  - ", file.path(out_models, "baseline_no_insulin_model.rds"), "\n", sep = "")
cat("  - ", file.path(out_figs, "roc_full.png"), "\n", sep = "")
cat("  - ", file.path(out_figs, "roc_no_insulin.png"), "\n", sep = "")
cat("Done.\n")
