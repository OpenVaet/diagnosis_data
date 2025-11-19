# Packages needed
library(readxl)
library(stringr)

# Create output directory if it doesn't exist
out_dir <- file.path("data", "interventions")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# Map of year range -> URL
year_urls <- c(
  "2024-2025" = "https://files.digital.nhs.uk/6D/C40538/hosp-epis-stat-admi-proc-2024-25-tab.xlsx",
  "2023-2024" = "https://files.digital.nhs.uk/92/DB66C9/hosp-epis-stat-admi-proc-2023-24-tab-v2.xlsx",
  "2022-2023" = "https://files.digital.nhs.uk/CB/515826/hosp-epis-stat-admi-proc-2022-23-tab-V2.xlsx",
  "2021-2022" = "https://files.digital.nhs.uk/FA/DA0567/hosp-epis-stat-admi-proc-2021-22-tab.xlsx",
  "2020-2021" = "https://files.digital.nhs.uk/A6/43CDC1/hosp-epis-stat-admi-proc-2020-21-tab.xlsx",
  "2019-2020" = "https://files.digital.nhs.uk/20/0864E6/hosp-epis-stat-admi-proc-2019-20-tab.xlsx",
  "2018-2019" = "https://files.digital.nhs.uk/77/0C8B3F/hosp-epis-stat-admi-proc-2018-19-tab.xlsx",
  "2017-2018" = "https://files.digital.nhs.uk/B6/E239FA/hosp-epis-stat-admi-proc-2017-18-tab.xlsx",
  "2016-2017" = "https://files.digital.nhs.uk/publication/7/g/hosp-epis-stat-admi-proc-2016-17-tab.xlsx",
  "2015-2016" = "https://files.digital.nhs.uk/publicationimport/pub22xxx/pub22378/hosp-epis-stat-admi-proc-2015-16-tab.xlsx",
  "2014-2015" = "https://files.digital.nhs.uk/publicationimport/pub19xxx/pub19124/hosp-epis-stat-admi-proc-2014-15-tab.xlsx"
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
out_dir_xlsx <- file.path("data", "interventions")
out_dir_csv  <- file.path("data", "interventions_csv")
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
    sheet     = "All Procedure 4 Character",
    col_names = FALSE
  )
  
  # Convert to data.frame for easier base-R handling
  df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE)
  
  # 1) Find the header row.
  #    Use the row whose first cell mentions "4 character"
  #    (works whether the full title is on one line or split over two).
  col1_lower <- tolower(as.character(df_raw[[1]]))

  header_row_idx <- which(str_detect(col1_lower, "4\\s*character"))[1]

  # Fallback: if that failed for some reason, look for "all procedures"
  if (is.na(header_row_idx)) {
    header_row_idx <- which(str_detect(col1_lower, "all procedures"))[1]
  }

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
  header[3] <- "na.1"
  header[4] <- "na.2"
  header[5] <- "na.3"
  header[6] <- "na.4"
  header[7] <- "na.5"
  header[8] <- "All procedures"
  header[9] <- "Main procedure"
  header[10] <- "Male"
  header[11] <- "Female"
  header[12] <- "Gender Unknown"
  header[13] <- "Mean age"
  header[14] <- "Age 0"
  header[15] <- "Age 1-4"
  header[16] <- "Age 5-9"
  header[17] <- "Age 10-14"
  header[18] <- "Age 15"
  header[19] <- "Age 16"
  header[20] <- "Age 17"
  header[21] <- "Age 18"
  header[22] <- "Age 19"
  header[23] <- "Age 20-24"
  header[24] <- "Age 25-29"
  header[25] <- "Age 30-34"
  header[26] <- "Age 35-39"
  header[27] <- "Age 40-44"
  header[28] <- "Age 45-49"
  header[29] <- "Age 50-54"
  header[30] <- "Age 55-59"
  header[31] <- "Age 60-64"
  header[32] <- "Age 65-69"
  header[33] <- "Age 70-74"
  header[34] <- "Age 75-79"
  header[35] <- "Age 80-84"
  header[36] <- "Age 85-89"
  header[37] <- "Age 90+"
  header[38] <- "Day case"
  header[39] <- "Emergency"
  header[40] <- "Elective"
  header[41] <- "Other"

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


