# Packages needed
library(readxl)
library(stringr)

# Create output directory if it doesn't exist
out_dir <- file.path("data", "diagnosis")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# Map of year range -> URL
year_urls <- c(
  "2024-2025" = "https://files.digital.nhs.uk/CC/EA025D/hosp-epis-stat-admi-diag-2024-25-tab.xlsx",
  "2023-2024" = "https://files.digital.nhs.uk/A5/5B8474/hosp-epis-stat-admi-diag-2023-24-tab.xlsx",
  "2022-2023" = "https://files.digital.nhs.uk/7A/DB1B00/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx",
  "2021-2022" = "https://files.digital.nhs.uk/0E/E70963/hosp-epis-stat-admi-diag-2021-22-tab.xlsx",
  "2020-2021" = "https://files.digital.nhs.uk/5B/AD892C/hosp-epis-stat-admi-diag-2020-21-tab.xlsx",
  "2019-2020" = "https://files.digital.nhs.uk/37/8D9781/hosp-epis-stat-admi-diag-2019-20-tab%20supp.xlsx",
  "2018-2019" = "https://files.digital.nhs.uk/1C/B2AD9B/hosp-epis-stat-admi-diag-2018-19-tab.xlsx",
  "2017-2018" = "https://files.digital.nhs.uk/B2/5CEC8D/hosp-epis-stat-admi-diag-2017-18-tab.xlsx",
  "2016-2017" = "https://files.digital.nhs.uk/publication/7/d/hosp-epis-stat-admi-diag-2016-17-tab.xlsx",
  "2015-2016" = "https://files.digital.nhs.uk/publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-diag-2015-16-tab.xlsx",
  "2014-2015" = "https://files.digital.nhs.uk/publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-diag-2014-15-tab.xlsx"
)

# Loop over each year range and download if needed
for (year_range in names(year_urls)) {
  url <- year_urls[[year_range]]
  destfile <- file.path(out_dir, paste0(year_range, ".xlsx"))
  
  if (file.exists(destfile)) {
    message("Already exists, skipping: ", destfile)
  } else {
    message("Downloading ", year_range, " from ", url)
    tryCatch(
      {
        download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
        message("Saved to: ", destfile)
      },
      error = function(e) {
        message("Failed to download ", year_range, ": ", conditionMessage(e))
      }
    )
  }
}

# Make sure output dir exists
out_dir_xlsx <- file.path("data", "diagnosis")
out_dir_csv  <- file.path("data", "diagnosis_csv")
if (!dir.exists(out_dir_csv)) {
  dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)
}

# ---- helper: clean names for CSV ----
clean_names_for_csv <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\+", "plus", x)
  x <- gsub("[^a-z0-9]+", "_", x)  # non-alnum -> _
  x <- gsub("_+", "_", x)          # collapse multiple _
  x <- gsub("^_|_$", "", x)        # trim leading/trailing _
  x
}

# ---- helper: convert one workbook to CSV ----
convert_diagnosis_file <- function(year_range) {
  xlsx_path <- file.path(out_dir_xlsx, paste0(year_range, ".xlsx"))
  csv_path  <- file.path(out_dir_csv,  paste0(year_range, ".csv"))
  
  if (file.exists(csv_path)) {
    message("CSV already exists, skipping: ", csv_path)
    return(invisible(NULL))
  }
  
  if (!file.exists(xlsx_path)) {
    warning("XLSX not found, skipping: ", xlsx_path)
    return(invisible(NULL))
  }
  
  message("Processing ", year_range, " ...")
  
  # Read sheet with no header (we’ll detect header & data manually)
  df_raw <- read_excel(
    path      = xlsx_path,
    sheet     = "All Diagnoses 4 Character",
    col_names = FALSE
  )
  
  # Convert to data.frame for easier base-R handling
  df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE)
  
  # 1) Find header row: the one with the table title in column 1
  header_row_idx <- which(
    df_raw[[1]] == "All diagnoses: 4 character code and description"
  )[1]
  
  if (is.na(header_row_idx)) {
    stop("Could not find header row in ", xlsx_path)
  }
  
  # 2) Find first data row: first row after header where col1 looks like an ICD code
  #    (e.g. A00.0, B12.3, etc.)
  icd_pattern <- "^[A-Z][0-9]{2}[A-Z0-9]?\\.[A-Z0-9]?$"
  candidate_rows <- seq(header_row_idx + 1, nrow(df_raw))
  data_start_idx <- candidate_rows[
    str_detect(df_raw[[1]][candidate_rows], icd_pattern) &
      !is.na(df_raw[[1]][candidate_rows])
  ][1]
  
  if (is.na(data_start_idx)) {
    stop("Could not find first ICD code row in ", xlsx_path)
  }
  
  # 3) Raw header row
  header <- as.character(df_raw[header_row_idx, ])
  
  # Force first two columns to be simple names
  header[1] <- "code"
  header[2] <- "description"
  
  # 4) Body: all rows from first data row down
  df_body <- df_raw[data_start_idx:nrow(df_raw), , drop = FALSE]
  
  # 5) Drop rows that are not part of the numeric table (footnotes etc):
  #    keep only rows where at least one column beyond the first two is non-NA.
  if (ncol(df_body) > 2) {
    has_data <- apply(!is.na(df_body[, -(1:2), drop = FALSE]), 1, any)
  } else {
    has_data <- rep(FALSE, nrow(df_body))
  }
  df_body <- df_body[has_data, , drop = FALSE]
  
  # 6) Drop columns that are completely empty AND have no header
  #    (e.g. those blank spacer columns before "All diagnoses")
  empty_cols <- sapply(df_body, function(col) all(is.na(col)))
  no_header  <- is.na(header) | header == ""
  keep_cols  <- !(empty_cols & no_header)
  
  header  <- header[keep_cols]
  df_body <- df_body[, keep_cols, drop = FALSE]
  
  # 7) Apply cleaned column names
  colnames(df_body) <- clean_names_for_csv(header)
  
  # 8) Write CSV
  write.csv(df_body, file = csv_path, row.names = FALSE, na = "")
  message("Written: ", csv_path)
}

# ---- run for all years you have locally ----
year_ranges <- c(
  "2014-2015",
  "2015-2016",
  "2016-2017",
  "2017-2018",
  "2018-2019",
  "2019-2020",
  "2020-2021",
  "2021-2022",
  "2022-2023",
  "2023-2024",
  "2024-2025"
)

for (yr in year_ranges) {
  convert_diagnosis_file(yr)
}

