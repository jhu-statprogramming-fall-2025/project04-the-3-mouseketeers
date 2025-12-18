# ============================================
# identifypt.R
# Purpose: Identify CGM-only patients:
#          PtIDs in cgm.txt but not in Pump_BasalRateChange/Pump_BolusDelivered
# Outputs:
#   data/processed/ptid_counts.csv
#   data/processed/cgm_only_ptids.csv
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

data_dir      <- here("data", "raw", "Data Files")
processed_dir <- here("data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

cgm_file   <- file.path(data_dir, "cgm.txt")
basal_file <- file.path(data_dir, "Pump_BasalRateChange.txt")
bolus_file <- file.path(data_dir, "Pump_BolusDelivered.txt")

read_ptids_only <- function(path, encodings = c("UTF-8", "UTF-16LE", "UTF-16")) {
  if (!file.exists(path)) stop("Missing required input file: ", path)
  
  for (enc in encodings) {
    out <- tryCatch(
      {
        df <- read_delim(
          path,
          delim = "|",
          locale = locale(encoding = enc),
          show_col_types = FALSE,
          progress = FALSE,
          col_types = cols(.default = col_skip(), PtID = col_character())
        )
        ids <- unique(df$PtID)
        ids <- ids[!is.na(ids) & ids != ""]
        return(ids)
      },
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }
  
  stop("Failed to read PtID from: ", path, " (encoding/delimiter mismatch).")
}

cat("========================================\n")
cat("Identifying CGM-only patients\n")
cat("Raw data dir:", data_dir, "\n")
cat("========================================\n\n")

cgm_ids   <- read_ptids_only(cgm_file,   encodings = c("UTF-16LE", "UTF-16", "UTF-8"))
basal_ids <- read_ptids_only(basal_file, encodings = c("UTF-8", "UTF-16LE", "UTF-16"))
bolus_ids <- read_ptids_only(bolus_file, encodings = c("UTF-8", "UTF-16LE", "UTF-16"))

pump_ids <- unique(c(basal_ids, bolus_ids))
cgm_only <- sort(setdiff(cgm_ids, pump_ids))

ptid_counts <- tibble(
  file = c("cgm.txt", "Pump_BasalRateChange.txt", "Pump_BolusDelivered.txt"),
  n_unique_ptids = c(length(cgm_ids), length(basal_ids), length(bolus_ids))
)

write_csv(ptid_counts, file.path(processed_dir, "ptid_counts.csv"))
write_csv(tibble(PtID = cgm_only), file.path(processed_dir, "cgm_only_ptids.csv"))

cat("Counts written: ", file.path(processed_dir, "ptid_counts.csv"), "\n", sep = "")
cat("CGM-only IDs written: ", file.path(processed_dir, "cgm_only_ptids.csv"), "\n\n", sep = "")
cat("Summary:\n")
cat("  Unique PtIDs in cgm.txt: ", length(cgm_ids), "\n", sep = "")
cat("  Unique PtIDs in pump (basal+bolus): ", length(pump_ids), "\n", sep = "")
cat("  CGM-only PtIDs: ", length(cgm_only), "\n", sep = "")
cat("========================================\n")
cat("identifypt complete!\n")
cat("========================================\n")
