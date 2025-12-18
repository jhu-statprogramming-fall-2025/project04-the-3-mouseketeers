# ============================================
# src/_paths.R
# Central, reproducible paths (no hard-coded absolute paths)
# Outputs: data/csv/
# Raw txt: data/raw/Data Files/  (or DCLP3_RAW_DIR override)
# ============================================

suppressPackageStartupMessages(library(here))

# Outputs (derived CSVs)
TABLES_DIR <- here("output", "tables")

# Other artifacts
FIG_DIR    <- here("output", "figures")
MODELS_DIR <- here("output", "models")

# Raw txt inputs (prefer inside repo; allow env override)
RAW_DIR <- Sys.getenv("DCLP3_RAW_DIR")
if (!nzchar(RAW_DIR)) {
  RAW_DIR <- here("data", "raw", "Data Files")
}

ensure_dirs <- function() {
  dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(FIG_DIR,    recursive = TRUE, showWarnings = FALSE)
  dir.create(MODELS_DIR, recursive = TRUE, showWarnings = FALSE)
}

raw <- function(...) file.path(RAW_DIR, ...)
tbl <- function(...) file.path(TABLES_DIR, ...)
fig <- function(...) file.path(FIG_DIR, ...)
mdl <- function(...) file.path(MODELS_DIR, ...)

assert_exists <- function(path, label = NULL) {
  if (!file.exists(path)) {
    msg <- if (!is.null(label)) paste0(label, " missing: ", path) else paste0("Missing: ", path)
    stop(msg, call. = FALSE)
  }
}

assert_raw_files <- function(files) {
  missing <- files[!file.exists(file.path(RAW_DIR, files))]
  if (length(missing) > 0) {
    stop(
      paste0(
        "Raw data files not found in RAW_DIR:\n  ",
        RAW_DIR,
        "\nMissing:\n  - ",
        paste(missing, collapse = "\n  - "),
        "\n\nFix:\n  (A) Put the raw .txt files under data/raw/Data Files/\n  OR\n  (B) Sys.setenv(DCLP3_RAW_DIR='...') to your raw folder.\n"
      ),
      call. = FALSE
    )
  }
}
