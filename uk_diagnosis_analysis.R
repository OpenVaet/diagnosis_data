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
out_dir_csv  <- file.path("output", "diagnosis_csv")
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

# ---- helper: clean the code values ----
clean_code_values <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  # specifically remove the nasty '‡' + following spaces at the start
  x <- gsub("^‡\\s*", "", x)
  
  # as an extra safety net, drop ANY non-alphanumeric junk at the start
  x <- gsub("^[^A-Za-z0-9]+", "", x)
  
  # if you want to be extra strict, you can uppercase everything
  x <- toupper(x)
  
  x
}

# ---- helper: convert one workbook to CSV ----
convert_diagnosis_file <- function(year_range) {
  xlsx_path <- file.path(out_dir, paste0(year_range, ".xlsx"))
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

### ===============================================================
### ANALYSIS ON THE GENERATED CSV FILES
### ===============================================================

# We assume year_ranges and out_dir_csv are already defined above.

# Only keep years for which CSV actually exists
available_years <- year_ranges[
  file.exists(file.path(out_dir_csv, paste0(year_ranges, ".csv")))
]

if (length(available_years) == 0) {
  stop("No diagnosis CSV files found in ", out_dir_csv)
}

# Helper: convert year interval "2014-2015" -> mid year 2015 (numeric)
interval_to_midyear <- function(x) {
  as.numeric(sub(".*-", "", x))
}

# ---- 1. Load all CSVs into a single long table ----

all_data_list <- list()

for (yr in available_years) {
  csv_path <- file.path(out_dir_csv, paste0(yr, ".csv"))
  message("Loading ", csv_path)
  
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  if (!("code" %in% names(df))) {
    warning("No 'code' column in ", csv_path, " – skipping this file.")
    next
  }
  if (!("all_diagnoses" %in% names(df))) {
    warning("No 'all_diagnoses' column in ", csv_path, " – skipping this file.")
    next
  }
  
  # Clean the code
  df$code <- clean_code_values(df$code)
  
  # Ensure numeric
  df$all_diagnoses <- as.numeric(df$all_diagnoses)
  
  if (!("description" %in% names(df))) {
    df$description <- NA_character_
  }
  
  df$year_interval <- yr
  df$year_mid      <- interval_to_midyear(yr)
  
  all_data_list[[yr]] <- df[, c("code", "description", "all_diagnoses",
                                "year_interval", "year_mid")]
}


all_data <- do.call(rbind, all_data_list)

# Make sure year_interval is character
all_data$year_interval <- as.character(all_data$year_interval)

# Total number of years we are considering
n_years_total <- length(available_years)

### ---------------------------------------------------------------
### 1. Report codes not present in all years
### ---------------------------------------------------------------

# Presence of each code in which year intervals
code_years_list   <- split(all_data$year_interval, all_data$code)
code_years_unique <- lapply(code_years_list, function(x) sort(unique(x)))
n_years_by_code   <- sapply(code_years_unique, length)

codes_not_all_years <- names(code_years_unique)[
  n_years_by_code < n_years_total
]

# Aggregate descriptions per code (some codes might have multiple descriptions across years)
desc_by_code <- tapply(
  all_data$description,
  all_data$code,
  function(x) {
    u <- unique(na.omit(x))
    if (length(u) == 0) NA_character_ else paste(u, collapse = " | ")
  }
)

report_not_all <- data.frame(
  code          = codes_not_all_years,
  description   = unname(desc_by_code[codes_not_all_years]),
  n_years       = n_years_by_code[codes_not_all_years],
  years_present = sapply(code_years_unique[codes_not_all_years],
                         paste, collapse = ", "),
  stringsAsFactors = FALSE
)

# Sort for readability
report_not_all <- report_not_all[order(report_not_all$n_years,
                                       report_not_all$code), ]

message("Number of codes NOT present in all years: ",
        nrow(report_not_all))

# Print a sample in console
print(utils::head(report_not_all, 50))

# Save full report to CSV
not_all_path <- file.path(out_dir_csv, "codes_not_in_all_years.csv")
write.csv(report_not_all, file = not_all_path, row.names = FALSE)
message("Full 'not in all years' report written to: ", not_all_path)

### ---------------------------------------------------------------
### 2. Trend analysis for codes present in ALL years
### ---------------------------------------------------------------

codes_all_years <- names(code_years_unique)[
  n_years_by_code == n_years_total
]

message("Number of codes present in ALL years: ",
        length(codes_all_years))

# We only consider something "abnormally high" if it is at least 5% above
# the upper prediction interval
anomaly_threshold <- 1.05

# Baseline years: 2015–2019 (i.e. intervals 2014-2015 ... 2018-2019)
baseline_years  <- 2015:2019
future_years    <- 2020:2024   # where we'll check for out-of-band increases

alerts_list <- list()
alert_idx <- 1

for (cd in codes_all_years) {
  df_code <- all_data[all_data$code == cd, ]
  
  # Drop rows with NA counts
  df_code <- df_code[!is.na(df_code$all_diagnoses), ]
  
  # Extract a single description (first non-NA)
  desc <- df_code$description[which(!is.na(df_code$description))[1]]
  if (is.na(desc)) desc <- ""
  
  # Baseline subset: years 2015–2019
  df_base <- df_code[df_code$year_mid %in% baseline_years, ]
  # Need at least 2 points to fit a line
  if (nrow(df_base) < 2) next
  
  # Linear regression of all_diagnoses ~ year_mid
  fit <- lm(all_diagnoses ~ year_mid, data = df_base)
  
  # Future years where we have actuals
  df_future <- df_code[df_code$year_mid %in% future_years, ]
  if (nrow(df_future) == 0) next
  
  # Predictions with 95% prediction intervals
  pred <- predict(
    fit,
    newdata  = data.frame(year_mid = df_future$year_mid),
    interval = "prediction",
    level    = 0.95
  )
  
  # Make sure prediction rows align with df_future rows
  df_future$pred       <- pred[, "fit"]
  df_future$pred_lwr   <- pred[, "lwr"]
  df_future$pred_upr   <- pred[, "upr"]
  
  # Identify years where actual value is ABOVE upper prediction bound
  idx_up <- which(df_future$all_diagnoses > df_future$pred_upr * anomaly_threshold)
  if (length(idx_up) == 0) next
  
  df_anomalies <- df_future[idx_up, ]
  df_anomalies$code        <- cd
  df_anomalies$description <- desc
  
  # Extra: how far above the upper bound (percent)
  df_anomalies$excess_pct <- 100 * (
    (df_anomalies$all_diagnoses / df_anomalies$pred_upr) - 1
  )
  
  alerts_list[[alert_idx]] <- df_anomalies
  alert_idx <- alert_idx + 1
}

if (length(alerts_list) == 0) {
  message("No codes with 2020–2024 all_diagnoses above 95% prediction interval.")
} else {
  alerts <- do.call(rbind, alerts_list)
  
  # Keep only informative columns and sort
  alerts_report <- alerts[, c(
    "code", "description",
    "year_interval", "year_mid",
    "all_diagnoses", "pred", "pred_lwr", "pred_upr",
    "excess_pct"
  )]
  
  alerts_report <- alerts_report[
    order(alerts_report$code, alerts_report$year_mid),
  ]
  
  # ---------------------------------------------------------------
  # Pattern classification helper
  # ---------------------------------------------------------------
  classify_pattern <- function(cd, all_data, baseline_years, future_years) {
    df_code <- all_data[all_data$code == cd & !is.na(all_data$all_diagnoses), ,
                        drop = FALSE]
    if (nrow(df_code) == 0) return(NA_character_)
    
    df_code <- df_code[order(df_code$year_mid), , drop = FALSE]
    
    # Baseline subset (2015–2019), as before
    df_base <- df_code[df_code$year_mid %in% baseline_years, , drop = FALSE]
    if (nrow(df_base) < 2) return(NA_character_)
    
    fit <- lm(all_diagnoses ~ year_mid, data = df_base)
    
    # Predictions + PI for ALL years for this code
    pred_all <- predict(
      fit,
      newdata  = data.frame(year_mid = df_code$year_mid),
      interval = "prediction",
      level    = 0.95
    )
    
    df_code$pred     <- pred_all[, "fit"]
    df_code$pred_lwr <- pred_all[, "lwr"]
    df_code$pred_upr <- pred_all[, "upr"]
    
    # Mark anomaly years in 2020–2024
    df_code$is_alert <- df_code$all_diagnoses > df_code$pred_upr * anomaly_threshold &
                    df_code$year_mid %in% future_years
    
    future_rows <- df_code$year_mid %in% future_years
    if (!any(df_code$is_alert[future_rows], na.rm = TRUE)) {
      return(NA_character_)
    }
    
    # Years (mid) in 2020–2024 and anomaly flags
    future_year_vals <- df_code$year_mid[future_rows]
    future_alerts    <- df_code$is_alert[future_rows]
    
    anomaly_years <- future_year_vals[future_alerts]
    anomaly_years <- sort(unique(anomaly_years))
    if (length(anomaly_years) == 0) return(NA_character_)
    
    # Detect starts of runs of anomalous years (e.g. 2020 and then 2022 in your example)
    run_starts <- anomaly_years[1]
    if (length(anomaly_years) > 1) {
      run_starts <- c(
        run_starts,
        anomaly_years[which(diff(anomaly_years) > 1) + 1]
      )
    }
    
    first_years       <- numeric(length(run_starts))
    returns_baselines <- logical(length(run_starts))
    
    for (k in seq_along(run_starts)) {
      fy <- run_starts[k]
      first_years[k] <- fy
      
      # Only look 1–2 years after the start of the run
      later_idx <- which(
        df_code$year_mid >  fy &
        df_code$year_mid <= fy + 2 &
        df_code$year_mid %in% future_years
      )
      
      # "Return to baseline" = later year (within next 1–2 years)
      # that is inside the 95% PI band and not an alert
      returns_baselines[k] <- length(later_idx) > 0 && any(
        !df_code$is_alert[later_idx] &
          df_code$all_diagnoses[later_idx] >= df_code$pred_lwr[later_idx] &
          df_code$all_diagnoses[later_idx] <= df_code$pred_upr[later_idx],
        na.rm = TRUE
      )
    }
    
    # Choose which run to base the label on:
    #  - Prefer the *latest* run that does NOT return to baseline (skyrocketing)
    #  - Otherwise use the earliest run (spike)
    if (any(!returns_baselines)) {
      chosen_year    <- max(first_years[!returns_baselines])
      chosen_returns <- FALSE
    } else {
      chosen_year    <- min(first_years)
      chosen_returns <- TRUE
    }
    
    # Map chosen_year + whether it returns to baseline -> label
    if (chosen_year == 2020) {
      if (chosen_returns) {
        # NEW: spike centred on 2019–2020
        "Spiking 2019-2020"
      } else {
        "Skyrocketting 2019-2020"
      }
    } else if (chosen_year == 2021) {
      if (chosen_returns) {
        "Spiking 2020-2021"
      } else {
        "Skyrocketting 2020-2021"
      }
    } else if (chosen_year == 2022) {
      # (You could add a "Spiking 2021-2022" case here if you wish)
      "Skyrocketting 2021-2022"
    } else if (chosen_year >= 2023) {
      "Skyrocketting Post 2022"
    } else {
      NA_character_
    }


  }

  
  # ---------------------------------------------------------------
  # 2a. Filter out events with < 1000 diagnoses in that year
  # ---------------------------------------------------------------
  min_events_threshold <- 1000
  alerts_report <- alerts_report[
    alerts_report$all_diagnoses >= min_events_threshold, 
  ]
  
  if (nrow(alerts_report) == 0) {
    message(
      "No alerts with all_diagnoses >= ", min_events_threshold,
      " – skipping HTML report generation."
    )
  } else {
    # Compute pattern classification per code (only for codes in filtered alerts)
    codes_for_patterns <- unique(alerts_report$code)
    pattern_by_code <- sapply(
      codes_for_patterns,
      classify_pattern,
      all_data       = all_data,
      baseline_years = baseline_years,
      future_years   = future_years
    )
    names(pattern_by_code) <- codes_for_patterns
    
    alerts_report$pattern_type <- unname(pattern_by_code[alerts_report$code])
    alerts_report$pattern_type[is.na(alerts_report$pattern_type)] <- ""

    # Unique pattern types (non-empty) for the dropdown
    pattern_options <- sort(unique(alerts_report$pattern_type))
    pattern_options <- pattern_options[nzchar(pattern_options)]

    # Reorder columns to put pattern near the front
    alerts_report <- alerts_report[, c(
      "code", "description", "pattern_type",
      "year_interval", "year_mid",
      "all_diagnoses", "pred", "pred_lwr", "pred_upr",
      "excess_pct"
    )]
    
    # Save filtered alerts as CSV (with pattern_type included)
    alerts_csv_path <- file.path(out_dir_csv, "diagnosis_trend_alerts.csv")
    write.csv(alerts_report, file = alerts_csv_path, row.names = FALSE)
    message("Trend alerts CSV written to: ", alerts_csv_path)
    
    ### ---- Build main HTML report ----
    html_path <- file.path(out_dir_csv, "diagnosis_trend_report.html")
    
    html_header <- paste0(
      "<!DOCTYPE html>\n",
      "<html><head><meta charset='UTF-8'>",
      "<title>Diagnosis Trend Report</title>",
      "<style>",
      "body { font-family: Arial, sans-serif; }",
      "table { border-collapse: collapse; width: 100%; }",
      "th, td { border: 1px solid #ccc; padding: 4px 8px; font-size: 12px; }",
      "th { background: #f0f0f0; }",
      ".code { font-family: monospace; }",
      ".pos { color: #b30000; font-weight: bold; }",
      "a { color: #004c99; text-decoration: none; }",
      "a:hover { text-decoration: underline; }",
      "</style>",
      "<script type='text/javascript'>",
      "function normalize(str){ return (str || '').toString().toLowerCase(); }",
      "function applyFilters(){",
      "  var pattern = normalize(document.getElementById('patternFilter').value);",
      "  var text = normalize(document.getElementById('textFilter').value);",
      "  var rows = document.querySelectorAll('#alertsTable tbody tr');",
      "  rows.forEach(function(row){",
      "    var p = normalize(row.getAttribute('data-pattern'));",
      "    var s = normalize(row.getAttribute('data-search'));",
      "    var matchPattern = !pattern || p === pattern;",
      "    var matchText = !text || s.indexOf(text) !== -1;",
      "    row.style.display = (matchPattern && matchText) ? '' : 'none';",
      "  });",
      "}",
      "document.addEventListener('DOMContentLoaded', function(){",
      "  var pf = document.getElementById('patternFilter');",
      "  if (pf) pf.addEventListener('change', applyFilters);",
      "  var tf = document.getElementById('textFilter');",
      "  if (tf) tf.addEventListener('keyup', applyFilters);",
      "});",
      "</script>",
      "</head><body>\n",
      "<h1>Diagnosis codes with 2020–2024 counts above 95% prediction interval</h1>\n",
      "<p>Baseline years for linear trend: 2015–2019 (intervals 2014–2015 to 2018–2019).</p>\n",
      "<p>Only years with at least ", min_events_threshold, 
      " diagnoses are shown in this report.</p>\n"
    )

    
    table_header <- paste0(
      # Pattern dropdown above the table
      "<div style='margin:10px 0;'>",
      "<label for='patternFilter'><strong>Pattern filter:</strong></label> ",
      "<select id='patternFilter'>",
      "<option value=''>All patterns</option>",
      paste(
        sapply(pattern_options, function(p) {
          sprintf(
            "<option value='%s'>%s</option>",
            htmltools::htmlEscape(p),
            htmltools::htmlEscape(p)
          )
        }),
        collapse = ""
      ),
      "</select>",
      "</div>\n",
      # Table with ID for JS
      "<table id='alertsTable'>\n",
      "<thead><tr>",
      "<th>Code</th>",
      "<th>Description<br/>",
      "<input type='text' id='textFilter' ",
      "placeholder='Search code/description...' ",
      "style='width:100%;box-sizing:border-box;font-size:11px;'/>",
      "</th>",
      "<th>Pattern</th>",
      "<th>Year interval</th>",
      "<th>Mid year</th>",
      "<th>Observed all_diagnoses</th>",
      "<th>Predicted</th>",
      "<th>Lower 95% PI</th>",
      "<th>Upper 95% PI</th>",
      "<th>% above upper PI</th>",
      "</tr></thead>\n<tbody>\n"
    )
    
    # Helper to make a safe ID/filename from a code
    make_code_id <- function(cd) {
      gsub("[^A-Za-z0-9]+", "_", cd)
    }
    
    # Build table rows with clickable code/description and pattern
    row_html <- apply(alerts_report, 1, function(r) {
      code_raw <- r[["code"]]
      code_id  <- make_code_id(code_raw)
      detail_file <- paste0("code_", code_id, ".html")
      
      desc_raw    <- ifelse(is.na(r[["description"]]), "", r[["description"]])
      pattern_raw <- ifelse(is.na(r[["pattern_type"]]), "", r[["pattern_type"]])
      
      code_link <- sprintf(
        "<a href='%s'>%s</a>",
        detail_file,
        htmltools::htmlEscape(code_raw)
      )
      desc_link <- sprintf(
        "<a href='%s'>%s</a>",
        detail_file,
        htmltools::htmlEscape(desc_raw)
      )
      pattern_html  <- htmltools::htmlEscape(pattern_raw)
      
      # Data attributes used by JS for filtering
      search_text <- tolower(paste(code_raw, desc_raw))
      pattern_attr <- htmltools::htmlEscape(pattern_raw)
      search_attr  <- htmltools::htmlEscape(search_text)
      
      sprintf(
        "<tr data-pattern='%s' data-search='%s'>
           <td class='code'>%s</td>
           <td>%s</td>
           <td>%s</td>
           <td>%s</td>
           <td>%s</td>
           <td>%s</td>
           <td>%0.1f</td>
           <td>%0.1f</td>
           <td>%0.1f</td>
           <td class='pos'>%0.1f%%</td>
         </tr>\n",
        pattern_attr,
        search_attr,
        code_link,
        desc_link,
        pattern_html,
        r[["year_interval"]],
        r[["year_mid"]],
        format(as.numeric(r[["all_diagnoses"]]), big.mark = ",", scientific = FALSE),
        as.numeric(r[["pred"]]),
        as.numeric(r[["pred_lwr"]]),
        as.numeric(r[["pred_upr"]]),
        as.numeric(r[["excess_pct"]])
      )
    })

    html_footer <- "</tbody>\n</table>\n</body></html>\n"
    
    # Write main HTML file
    cat(html_header, table_header, paste(row_html, collapse = ""),
        html_footer, file = html_path)
    
    message("HTML trend report written to: ", html_path)
    
    # -------------------------------------------------------------
    # 2b. Per-code detail pages with charts & full time-series
    # -------------------------------------------------------------
    
    codes_for_details <- unique(alerts_report$code)
    
    for (cd in codes_for_details) {
      # All data for this code
      df_code <- all_data[all_data$code == cd, , drop = FALSE]
      df_code <- df_code[!is.na(df_code$all_diagnoses), , drop = FALSE]
      df_code <- df_code[order(df_code$year_mid), , drop = FALSE]
      
      # Description (first non-NA)
      desc <- df_code$description[which(!is.na(df_code$description))[1]]
      if (is.na(desc)) desc <- ""
      
      # Pattern from precomputed vector
      pattern <- pattern_by_code[[cd]]
      if (is.null(pattern) || is.na(pattern)) pattern <- ""
      
      # Baseline subset & model (same as before)
      df_base <- df_code[df_code$year_mid %in% baseline_years, , drop = FALSE]
      if (nrow(df_base) < 2) next  # defensive
      
      fit <- lm(all_diagnoses ~ year_mid, data = df_base)
      
      # Predictions + PI for ALL years for this code
      pred_all <- predict(
        fit,
        newdata  = data.frame(year_mid = df_code$year_mid),
        interval = "prediction",
        level    = 0.95
      )
      
      df_code$pred     <- pred_all[, "fit"]
      df_code$pred_lwr <- pred_all[, "lwr"]
      df_code$pred_upr <- pred_all[, "upr"]
      
      # Mark anomaly years (using same criteria as main analysis)
      df_code$is_alert <- df_code$all_diagnoses > df_code$pred_upr * anomaly_threshold &
                          df_code$year_mid %in% future_years
      
      # ----------------- PLOT (PNG) -----------------
      code_id   <- make_code_id(cd)
      png_file  <- file.path(out_dir_csv, paste0("trend_", code_id, ".png"))
      
      y_min <- min(c(df_code$all_diagnoses, df_code$pred_lwr), na.rm = TRUE)
      y_max <- max(c(df_code$all_diagnoses, df_code$pred_upr), na.rm = TRUE)
      
      png(png_file, width = 800, height = 600)
      par(mar = c(5, 5, 5, 2))
      
      plot(
        df_code$year_mid, df_code$all_diagnoses,
        type = "b", pch = 16,
        xlab = "Year (mid-point)",
        ylab = "All diagnoses",
        main = paste0(cd, " - ", substr(desc, 1, 80)),
        ylim = c(y_min, y_max)
      )
      # Predicted line
      lines(df_code$year_mid, df_code$pred, lty = 2)
      # Prediction interval
      lines(df_code$year_mid, df_code$pred_lwr, lty = 3)
      lines(df_code$year_mid, df_code$pred_upr, lty = 3)
      
      # Labels on each point
      text(
        df_code$year_mid, df_code$all_diagnoses,
        labels = format(df_code$all_diagnoses, big.mark = ",", scientific = FALSE),
        pos = 3, cex = 0.7
      )
      
      legend(
        "topleft",
        legend = c("Observed", "Predicted", "95% PI"),
        lty    = c(1, 2, 3),
        pch    = c(16, NA, NA),
        bty    = "n"
      )
      
      dev.off()
      
      # ----------------- DETAIL HTML -----------------
      detail_file <- file.path(out_dir_csv, paste0("code_", code_id, ".html"))
      
      df_table <- df_code[, c(
        "year_interval", "year_mid",
        "all_diagnoses", "pred", "pred_lwr", "pred_upr", "is_alert"
      )]
      
      table_rows <- apply(df_table, 1, function(r) {
        sprintf(
          "<tr>
             <td>%s</td>
             <td>%s</td>
             <td>%s</td>
             <td>%0.1f</td>
             <td>%0.1f</td>
             <td>%0.1f</td>
             <td>%s</td>
           </tr>\n",
          r[["year_interval"]],
          r[["year_mid"]],
          format(as.numeric(r[["all_diagnoses"]]), big.mark = ",", scientific = FALSE),
          as.numeric(r[["pred"]]),
          as.numeric(r[["pred_lwr"]]),
          as.numeric(r[["pred_upr"]]),
          ifelse(as.logical(r[["is_alert"]]), "Yes", "")
        )
      })
      
      detail_html <- paste0(
        "<!DOCTYPE html>\n<html><head><meta charset='UTF-8'>",
        "<title>Trend detail for ", htmltools::htmlEscape(cd), "</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; }",
        "table { border-collapse: collapse; width: 100%; margin-top: 16px; }",
        "th, td { border: 1px solid #ccc; padding: 4px 8px; font-size: 12px; }",
        "th { background: #f0f0f0; }",
        ".code { font-family: monospace; }",
        ".alert { background-color: #ffe6e6; }",
        "a { color: #004c99; text-decoration: none; }",
        "a:hover { text-decoration: underline; }",
        "</style>",
        "</head><body>",
        "<h1>Trend detail for code ", htmltools::htmlEscape(cd), "</h1>",
        "<p><strong>Description:</strong> ", htmltools::htmlEscape(desc), "</p>",
        if (nzchar(pattern)) paste0(
          "<p><strong>Pattern classification:</strong> ",
          htmltools::htmlEscape(pattern), "</p>"
        ) else "",
        "<p><a href='diagnosis_trend_report.html'>&larr; Back to main report</a></p>",
        "<img src='", basename(png_file), 
        "' alt='Trend chart for ", htmltools::htmlEscape(cd), 
        "' style='max-width:100%;height:auto;border:1px solid #ccc;' />",
        "<h2>Yearly data</h2>",
        "<table>",
        "<tr>",
        "<th>Year interval</th>",
        "<th>Mid year</th>",
        "<th>Observed all_diagnoses</th>",
        "<th>Predicted</th>",
        "<th>Lower 95% PI</th>",
        "<th>Upper 95% PI</th>",
        "<th>Anomaly (above upper PI)</th>",
        "</tr>",
        paste(table_rows, collapse = ""),
        "</table>",
        "</body></html>"
      )
      
      cat(detail_html, file = detail_file)
      message("Detail HTML written for code ", cd, ": ", detail_file)
    }
  }
}
