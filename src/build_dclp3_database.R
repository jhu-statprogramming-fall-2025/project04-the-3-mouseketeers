# ============================================
# src/build_dclp3_database.R
# Purpose: Database programming paradigm (DBI + SQLite)
#  - Ingest key DCLP3 raw text files into SQLite: data/dclp3.sqlite
#  - Save distinct PtID counts per table: output/tables/db_table_counts.csv
#  - (Optional) Export CGM-only PtIDs from SQL: data/processed/cgm_only_ptids_from_db.csv
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

db_path   <- here("data", "dclp3.sqlite")
data_dir  <- here("data", "raw", "Data Files")
out_dir   <- here("output", "tables")
proc_dir  <- here("data", "processed")

dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir,          recursive = TRUE, showWarnings = FALSE)
dir.create(proc_dir,         recursive = TRUE, showWarnings = FALSE)

read_dclp_file <- function(path, encoding = "UTF-16LE", delim = "|") {
  read_delim(
    path,
    delim = delim,
    locale = locale(encoding = encoding),
    na = c("", "NA", " "),
    show_col_types = FALSE,
    progress = FALSE
  )
}

ingest_table <- function(con, filename, tablename, encoding) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) {
    warning("Missing file (skipping): ", filename)
    return(FALSE)
  }
  
  message("Ingesting ", filename, " -> ", tablename)
  df <- read_dclp_file(path, encoding = encoding)
  
  dbWriteTable(con, tablename, df, overwrite = TRUE)
  
  if ("PtID" %in% names(df)) {
    dbExecute(con, sprintf(
      "CREATE INDEX IF NOT EXISTS idx_%s_PtID ON %s(PtID)",
      tablename, tablename
    ))
  }
  
  TRUE
}

# Connect (DBI: dbConnect/dbWriteTable/dbGetQuery/dbDisconnect) :contentReference[oaicite:3]{index=3}
con <- dbConnect(SQLite(), dbname = db_path)
on.exit(dbDisconnect(con), add = TRUE)

# File list mirrors your current codebase’s raw inputs (UTF-16LE for Diab*/Medical*/Medication*). :contentReference[oaicite:4]{index=4}
tables <- list(
  screening         = list(file = "DiabScreening_a.txt",    enc = "UTF-16LE"),
  phys_exam         = list(file = "DiabPhysExam_a.txt",     enc = "UTF-16LE"),
  socio_econ        = list(file = "DiabSocioEcon_a.txt",    enc = "UTF-16LE"),
  hba1c             = list(file = "DiabLocalHbA1c_a.txt",   enc = "UTF-16LE"),
  medical_condition = list(file = "MedicalCondition_a.txt", enc = "UTF-16LE"),
  medication        = list(file = "Medication_a.txt",       enc = "UTF-16LE"),
  
  # Pump/CGM logs (these encodings match the prior review’s suggested DB build). :contentReference[oaicite:5]{index=5}
  pump_basal        = list(file = "Pump_BasalRateChange.txt", enc = "UTF-8"),
  pump_bolus        = list(file = "Pump_BolusDelivered.txt",  enc = "UTF-8"),
  pump_cgm          = list(file = "Pump_CGMGlucoseValue.txt", enc = "UTF-8"),
  cgm_txt           = list(file = "cgm.txt",                  enc = "UTF-16LE")
)

ingested <- character(0)
for (tbl in names(tables)) {
  spec <- tables[[tbl]]
  ok <- ingest_table(con, spec$file, tbl, spec$enc)
  if (isTRUE(ok)) ingested <- c(ingested, tbl)
}

# Distinct PtID counts (as in your prior code review suggestion). :contentReference[oaicite:6]{index=6}
counts <- bind_rows(lapply(ingested, function(tbl) {
  fields <- dbListFields(con, tbl)
  if (!("PtID" %in% fields)) {
    return(tibble(table = tbl, n_ptids = NA_integer_))
  }
  q <- sprintf("SELECT COUNT(DISTINCT PtID) AS n_ptids FROM %s", tbl)
  res <- dbGetQuery(con, q)
  tibble(table = tbl, n_ptids = res$n_ptids)
}))

write_csv(counts, file.path(out_dir, "db_table_counts.csv"))

# OPTIONAL: CGM-only PtIDs using SQL set logic (cgm_txt minus any pump table PtIDs)
if (all(c("cgm_txt", "pump_basal", "pump_bolus") %in% ingested)) {
  q_cgm_only <- "
    SELECT DISTINCT c.PtID
    FROM cgm_txt c
    LEFT JOIN (
      SELECT DISTINCT PtID FROM pump_basal
      UNION
      SELECT DISTINCT PtID FROM pump_bolus
      UNION
      SELECT DISTINCT PtID FROM pump_cgm
    ) p
    ON c.PtID = p.PtID
    WHERE p.PtID IS NULL
  "
  cgm_only <- dbGetQuery(con, q_cgm_only)
  write_csv(cgm_only, file.path(proc_dir, "cgm_only_ptids_from_db.csv"))
}

message("Database built at: ", db_path)
message("Saved: ", file.path(out_dir, "db_table_counts.csv"))
message("Optional: ", file.path(proc_dir, "cgm_only_ptids_from_db.csv"))
