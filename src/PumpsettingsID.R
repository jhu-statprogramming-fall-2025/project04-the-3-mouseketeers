# ============================================
# PumpsettingsID.R
# Purpose: Compare CGM-only PtIDs vs InsulinPumpSettings_a PtIDs (sanity check)
# Input:
#   data/raw/Data Files/InsulinPumpSettings_a.txt
#   data/processed/cgm_only_ptids.csv
# Output:
#   output/tables/pumpsettings_vs_cgm_only_counts.csv
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

data_dir      <- here("data", "raw", "Data Files")
processed_dir <- here("data", "processed")
out_tables    <- here("output", "tables")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)

pump_settings_file <- file.path(data_dir, "InsulinPumpSettings_a.txt")
cgm_only_file      <- file.path(processed_dir, "cgm_only_ptids.csv")

if (!file.exists(pump_settings_file)) stop("Missing file: ", pump_settings_file)
if (!file.exists(cgm_only_file)) stop("Missing file: ", cgm_only_file)

read_ptids_table <- function(path, encoding = "UTF-16LE") {
  df <- read_delim(
    path,
    delim = "|",
    locale = locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE,
    col_types = cols(.default = col_skip(), PtID = col_character())
  )
  ids <- unique(df$PtID)
  ids <- ids[!is.na(ids) & ids != ""]
  ids
}

pump_ids <- read_ptids_table(pump_settings_file, encoding = "UTF-16LE")
cgm_only_ids <- read_csv(cgm_only_file, show_col_types = FALSE,
                         col_types = cols(PtID = col_character())) %>%
  pull(PtID) %>%
  unique() %>%
  (\(x) x[!is.na(x) & x != ""])()

matches              <- sort(intersect(pump_ids, cgm_only_ids))
in_cgm_only_not_file  <- sort(setdiff(cgm_only_ids, pump_ids))
in_file_not_cgm_only  <- sort(setdiff(pump_ids, cgm_only_ids))

cat("========================================\n")
cat("PumpsettingsID sanity check\n")
cat("========================================\n")
cat("Matching IDs (in both): ", length(matches), "\n", sep = "")
cat("In CGM-only but NOT pump settings: ", length(in_cgm_only_not_file), "\n", sep = "")
cat("In pump settings but NOT CGM-only: ", length(in_file_not_cgm_only), "\n", sep = "")

counts <- tibble(
  group = c("matches", "cgm_only_not_pumpsettings", "pumpsettings_not_cgm_only"),
  n = c(length(matches), length(in_cgm_only_not_file), length(in_file_not_cgm_only))
)

write_csv(counts, file.path(out_tables, "pumpsettings_vs_cgm_only_counts.csv"))
cat("Wrote: ", file.path(out_tables, "pumpsettings_vs_cgm_only_counts.csv"), "\n", sep = "")
