# ============================================
# src/predict_cgm_only_patients.R
# Robust CGM-only prediction:
#  - avoids preProcess column/type mismatch
#  - avoids model.matrix contrasts error (1-level factors)
#  - aligns newx to training design-matrix columns
# Outputs (data/csv):
#  - cgm_only_predictions.csv
#  - cgm_only_performance_metrics.csv
#  and optional ROC PNG (output/figures)
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(pROC)
  library(glmnet)
  library(tibble)
})

source(here("src", "_paths.R"))
ensure_dirs()

in_cgm_only <- tbl("combined_cgm_only_patient_data.csv")
model_file  <- mdl("baseline_no_insulin_model.rds")

out_pred    <- tbl("cgm_only_predictions.csv")
out_metrics <- tbl("cgm_only_performance_metrics.csv")
out_roc     <- fig("roc_cgm_only.png")

assert_exists(in_cgm_only, "Input")
assert_exists(model_file,  "Model")

df  <- read_csv(in_cgm_only, show_col_types = FALSE)
art <- readRDS(model_file)

if (is.null(art$cv_lasso)) {
  stop("baseline_no_insulin_model.rds is missing cv_lasso. Re-run ml_auc_models.R to regenerate.", call. = FALSE)
}

# Required training design-matrix columns (44 in your case)
train_cols <- art$train_cols
if (is.null(train_cols)) {
  cn <- rownames(coef(art$cv_lasso, s = "lambda.min"))
  train_cols <- setdiff(cn, "(Intercept)")
}
cat("Model expects ", length(train_cols), " design-matrix columns.\n", sep = "")

# ----------------------------
# Build predictors (drop ID/outcome/pred cols)
# ----------------------------
drop_cols <- c(
  "PtID",
  "pct_70_180",
  "Uncontrolled",
  "pred_prob_uncontrolled",
  "pred_uncontrolled",
  "actual_uncontrolled"
)

x_proc <- df %>% select(-any_of(drop_cols))

# ----------------------------
# Manual median imputation using TRAIN medians (when available)
# Avoids predict(preProc, ...) failing due to mismatched columns/types
# ----------------------------
if (!is.null(art$preProc) && !is.null(art$preProc$median)) {
  med <- art$preProc$median  # named numeric vector
  for (nm in names(med)) {
    if (nm %in% names(x_proc)) {
      x_proc[[nm]] <- suppressWarnings(as.numeric(x_proc[[nm]]))
      x_proc[[nm]][is.na(x_proc[[nm]])] <- med[[nm]]
    }
  }
}

# ----------------------------
# SAFE FACTORS: prevent "contrasts" error
# Ensure every categorical variable has >=2 levels
# ----------------------------
make_safe_factor <- function(v) {
  v <- as.character(v)
  v[is.na(v) | v == ""] <- "Unknown"
  lv <- unique(v)
  if (length(lv) < 2) lv <- c(lv, "__OTHER__")  # add an unused level so contrasts work
  factor(v, levels = lv)
}

for (col in names(x_proc)) {
  if (is.character(x_proc[[col]]) || is.factor(x_proc[[col]])) {
    x_proc[[col]] <- make_safe_factor(x_proc[[col]])
  }
}

# ----------------------------
# Model matrix WITHOUT silent row dropping
# ----------------------------
tmp <- bind_cols(x_proc, Uncontrolled = 0L)

x_mat <- model.matrix(Uncontrolled ~ ., data = tmp, na.action = na.pass)
x_mat <- x_mat[, -1, drop = FALSE]  # remove intercept, keep matrix

keep <- complete.cases(x_mat)
if (!all(keep)) {
  message("NOTE: ", sum(!keep), " rows still have NA after preprocessing; predictions will be NA for those rows.")
}
x_keep <- x_mat[keep, , drop = FALSE]

# ----------------------------
# Align to training columns (glmnet requires same p)
# ----------------------------
x_keep <- x_keep[, intersect(train_cols, colnames(x_keep)), drop = FALSE]

missing_cols <- setdiff(train_cols, colnames(x_keep))
if (length(missing_cols) > 0) {
  zeros <- matrix(0, nrow = nrow(x_keep), ncol = length(missing_cols),
                  dimnames = list(NULL, missing_cols))
  x_keep <- cbind(x_keep, zeros)
}
x_keep <- x_keep[, train_cols, drop = FALSE]

# Predict
threshold <- if (!is.null(art$threshold)) art$threshold else 0.5
prob_keep <- as.numeric(predict(art$cv_lasso, newx = x_keep, s = "lambda.min", type = "response"))

prob <- rep(NA_real_, nrow(df))
prob[keep] <- prob_keep

pred <- ifelse(is.na(prob), NA_integer_, as.integer(prob > threshold))

# Actual (if present) for evaluation only
has_outcome <- "pct_70_180" %in% names(df)
actual <- if (has_outcome) {
  if_else(is.na(df$pct_70_180), NA_integer_, if_else(df$pct_70_180 < 70, 1L, 0L))
} else {
  rep(NA_integer_, nrow(df))
}

# Save predictions
pred_df <- df %>%
  mutate(
    pred_prob_uncontrolled = prob,
    pred_uncontrolled = pred,
    actual_uncontrolled = actual
  )
write_csv(pred_df, out_pred)

# Metrics (only where both prob + actual exist)
metrics <- tibble(
  n_total = nrow(df),
  n_scored = sum(!is.na(prob)),
  threshold = threshold,
  accuracy_pct = NA_real_,
  auc = NA_real_
)

keep_eval <- !is.na(actual) & !is.na(prob)
if (any(keep_eval)) {
  metrics$accuracy_pct <- mean(pred[keep_eval] == actual[keep_eval]) * 100
  
  if (length(unique(actual[keep_eval])) >= 2) {
    auc_val <- as.numeric(auc(roc_obj))
    metrics$auc <- auc_val
    
    png(out_roc, width = 1100, height = 800)
    par(mar = c(4.5, 4.5, 3, 1))
    plot(
      roc_obj,
      col = "#10b981",
      lwd = 4,
      legacy.axes = TRUE,
      main = "ROC curve — Baseline transfer to CGM-only cohort",
      xlab = "False positive rate (1 - specificity)",
      ylab = "True positive rate (sensitivity)"
    )
    grid(col = "#d1d5db")
    text(0.62, 0.18, labels = paste0("AUC = ", round(auc_val, 3)), cex = 1.2)
    dev.off()
  } else {
    message("NOTE: AUC skipped (eval set has only one class).")
  }
}

write_csv(metrics, out_metrics)

cat("Saved:\n")
cat("  - ", out_pred, "\n", sep = "")
cat("  - ", out_metrics, "\n", sep = "")
if (file.exists(out_roc)) cat("  - ", out_roc, "\n", sep = "")
