# Packages needed
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr) 

# Create output directory if it doesn't exist
data_dir_csv  <- file.path("data", "sw", "diagnosis_csv")
out_dir_report  <- file.path("output", "sw", "diagnosis_report")
if (!dir.exists(out_dir_report)) {
  dir.create(out_dir_report, recursive = TRUE, showWarnings = FALSE)
}

# ---- Load Swedish diagnosis data ----
message("Loading Swedish diagnosis data...")
sw_data_path <- file.path("data", "sw", "diagnosis_csv", "diagnosis_2008_2024.csv")

if (!file.exists(sw_data_path)) {
  stop("Swedish data file not found: ", sw_data_path)
}

sw_raw <- read.csv(sw_data_path, stringsAsFactors = FALSE, check.names = FALSE)

# Check structure
message("Data columns: ", paste(names(sw_raw), collapse = ", "))
message("Unique measures: ", paste(unique(sw_raw$measure), collapse = ", "))
message("Unique ages: ", paste(unique(sw_raw$age_group), collapse = ", "))

# ---- Clean and prepare data ----

# Extract diagnosis code from 'diagnos' column (e.g., "A00 Cholera" -> "A00")
sw_raw$code <- sub("\\s.*$", "", sw_raw$diagnos)
sw_raw$description <- sub("^[A-Z][0-9]{2,3}\\s+", "", sw_raw$diagnos)

# Ensure numeric value
sw_raw$value <- as.numeric(sw_raw$value)

# Filter for "Number of patients" for overall trend analysis
all_data <- sw_raw %>%
  filter(measure == "Number of patients") %>%
  mutate(
    year_mid = as.numeric(year),
    year_interval = as.character(year)
  ) %>%
  select(
    code,
    description,
    age_group,
    all_diagnoses = value,
    year_interval,
    year_mid
  )

# Calculate total across all age groups for each code/year
all_data_total <- all_data %>%
  group_by(code, description, year_interval, year_mid) %>%
  summarise(
    all_diagnoses = sum(all_diagnoses, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Prepare age-specific data with rates per 100k ----
diag_age <- sw_raw %>%
  filter(measure == "Number of patients per 100,000 inhabitants") %>%
  mutate(
    year_mid = as.numeric(year),
    year_interval = as.character(year),
    code = sub("\\s.*$", "", diagnos),
    rate_per_100k = as.numeric(value)
  ) %>%
  select(
    code,
    age_group,
    year_interval,
    year_mid,
    rate_per_100k
  )

# Also get absolute cases for age groups
diag_age_cases <- sw_raw %>%
  filter(measure == "Number of patients") %>%
  mutate(
    year_mid = as.numeric(year),
    code = sub("\\s.*$", "", diagnos)
  ) %>%
  select(code, age_group, year_mid, cases = value)

# Merge cases and rates
diag_age <- diag_age %>%
  left_join(diag_age_cases, by = c("code", "age_group", "year_mid"))

# Get unique age groups in order
age_group_order <- unique(sw_raw$age_group)
message("Swedish age groups: ", paste(age_group_order, collapse = ", "))

# Total number of years we are considering
available_years <- sort(unique(all_data_total$year_mid))
n_years_total <- length(available_years)

message("Analysis years: ", paste(available_years, collapse = ", "))
message("Total years: ", n_years_total)

### ---------------------------------------------------------------
### Report codes not present in all years
### ---------------------------------------------------------------

# Presence of each code in which years
code_years_list   <- split(all_data_total$year_interval, all_data_total$code)
code_years_unique <- lapply(code_years_list, function(x) sort(unique(x)))
n_years_by_code   <- sapply(code_years_unique, length)

codes_not_all_years <- names(code_years_unique)[
  n_years_by_code < n_years_total
]

# Aggregate descriptions per code
desc_by_code <- tapply(
  all_data_total$description,
  all_data_total$code,
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
if (nrow(report_not_all) > 0) {
  print(utils::head(report_not_all, 50))
  
  # Save full report to CSV
  not_all_path <- file.path(data_dir_csv, "codes_not_in_all_years.csv")
  write.csv(report_not_all, file = not_all_path, row.names = FALSE)
  message("Full 'not in all years' report written to: ", not_all_path)
}

### ---------------------------------------------------------------
### Trend analysis for codes present in ALL years
### ---------------------------------------------------------------

codes_all_years <- names(code_years_unique)[
  n_years_by_code == n_years_total
]

message("Number of codes present in ALL years: ",
        length(codes_all_years))

# Anomaly threshold (5% above upper prediction interval)
anomaly_threshold <- 1.05

# Baseline years: 2008–2019
baseline_years  <- 2008:2019
future_years    <- 2020:2024   # where we'll check for out-of-band increases

alerts_list <- list()
alert_idx <- 1

for (cd in codes_all_years) {
  df_code <- all_data_total[all_data_total$code == cd, ]
  
  # Drop rows with NA counts
  df_code <- df_code[!is.na(df_code$all_diagnoses), ]
  
  # Extract a single description (first non-NA)
  desc <- df_code$description[which(!is.na(df_code$description))[1]]
  if (is.na(desc)) desc <- ""
  
  # Baseline subset: years 2008–2019
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
    
    # Baseline subset
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
    
    # Detect starts of runs of anomalous years
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
      if (chosen_returns) {
        "Spiking 2021-2022"
      } else {
        "Skyrocketting 2021-2022"
      }
    } else if (chosen_year >= 2023) {
      "Skyrocketting Post 2022"
    } else {
      NA_character_
    }
  }
  
  # ---------------------------------------------------------------
  # Filter out events with < 100 diagnoses in that year
  # ---------------------------------------------------------------
  min_events_threshold <- 100
  alerts_report <- alerts_report[
    alerts_report$all_diagnoses >= min_events_threshold, 
  ]
  
  if (nrow(alerts_report) == 0) {
    message(
      "No alerts with all_diagnoses >= ", min_events_threshold,
      " – skipping HTML report generation."
    )
  } else {
    # Compute pattern classification per code
    codes_for_patterns <- unique(alerts_report$code)
    pattern_by_code <- sapply(
      codes_for_patterns,
      classify_pattern,
      all_data       = all_data_total,
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
    
    # Save filtered alerts as CSV
    alerts_csv_path <- file.path(out_dir_report, "diagnosis_trend_alerts.csv")
    write.csv(alerts_report, file = alerts_csv_path, row.names = FALSE)
    message("Trend alerts CSV written to: ", alerts_csv_path)
    
    ### ---- Build main HTML report ----
    html_path <- file.path(out_dir_report, "diagnosis_trend_report.html")
    
    html_header <- paste0(
      "<!DOCTYPE html>\n",
      "<html><head><meta charset='UTF-8'>",
      "<title>Swedish Diagnosis Trend Report</title>",
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
      "<h1>Swedish Diagnosis codes with 2020–2024 counts above 95% prediction interval</h1>\n",
      "<p>Baseline years for linear trend: 2008–2019.</p>\n",
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
      "<th>Year</th>",
      "<th>Observed diagnoses</th>",
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
    # Per-code detail pages with charts & full time-series
    # -------------------------------------------------------------
    
    codes_for_details <- unique(alerts_report$code)
    
    for (cd in codes_for_details) {
      # All data for this code (all ages)
      df_code <- all_data_total[all_data_total$code == cd, , drop = FALSE]
      df_code <- df_code[!is.na(df_code$all_diagnoses), , drop = FALSE]
      df_code <- df_code[order(df_code$year_mid), , drop = FALSE]
      
      # Description (first non-NA)
      desc <- df_code$description[which(!is.na(df_code$description))[1]]
      if (is.na(desc)) desc <- ""
      
      # Pattern from precomputed vector
      pattern <- pattern_by_code[[cd]]
      if (is.null(pattern) || is.na(pattern)) pattern <- ""
      
      # Baseline subset & model
      df_base <- df_code[df_code$year_mid %in% baseline_years, , drop = FALSE]
      if (nrow(df_base) < 2) next
      
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
      
      # Mark anomaly years
      df_code$is_alert <- df_code$all_diagnoses > df_code$pred_upr * anomaly_threshold &
                          df_code$year_mid %in% future_years
      
      # ----------------- PLOT (all ages) -----------------
      code_id   <- make_code_id(cd)
      png_file  <- file.path(out_dir_report, paste0("trend_", code_id, ".png"))
      
      y_min <- min(c(df_code$all_diagnoses, df_code$pred_lwr), na.rm = TRUE)
      y_max <- max(c(df_code$all_diagnoses, df_code$pred_upr), na.rm = TRUE)
      
      png(png_file, width = 800, height = 600)
      par(mar = c(5, 5, 5, 2))
      
      plot(
        df_code$year_mid, df_code$all_diagnoses,
        type = "b", pch = 16,
        xlab = "Year",
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
      
      # ----------------- AGE-SPECIFIC BREAKDOWN -----------------
      age_section_html <- ""
      
      if (!is.null(diag_age)) {
        df_code_age <- diag_age[diag_age$code == cd, , drop = FALSE]
        
        if (nrow(df_code_age) > 0) {
          # Order by year
          df_code_age <- df_code_age %>%
            arrange(age_group, year_mid)

          # Create one trend plot per age group: cases per 100k
          age_groups <- unique(df_code_age$age_group)
          age_plots_html <- c()
          
          for (ag in age_groups) {
            df_ag <- df_code_age[df_code_age$age_group == ag, , drop = FALSE]
            df_ag <- df_ag[!is.na(df_ag$rate_per_100k), , drop = FALSE]
            if (nrow(df_ag) == 0) next
            
            # Ensure ordered by year
            df_ag <- df_ag[order(df_ag$year_mid), , drop = FALSE]
            
            # ---- Fit age-specific trend over baseline years ----
            df_base_ag <- df_ag[df_ag$year_mid %in% baseline_years &
                                  !is.na(df_ag$rate_per_100k), , drop = FALSE]
            
            if (nrow(df_base_ag) >= 2) {
              fit_ag <- lm(rate_per_100k ~ year_mid, data = df_base_ag)
              
              pred_all_ag <- predict(
                fit_ag,
                newdata  = data.frame(year_mid = df_ag$year_mid),
                interval = "prediction",
                level    = 0.95
              )
              
              df_ag$pred_rate <- pred_all_ag[, "fit"]
              df_ag$pred_lwr  <- pred_all_ag[, "lwr"]
              df_ag$pred_upr  <- pred_all_ag[, "upr"]
              
              y_min_ag <- min(df_ag$rate_per_100k, df_ag$pred_lwr, na.rm = TRUE)
              y_max_ag <- max(df_ag$rate_per_100k, df_ag$pred_upr, na.rm = TRUE)
            } else {
              # Not enough points to fit an age-specific trend
              df_ag$pred_rate <- NA_real_
              df_ag$pred_lwr  <- NA_real_
              df_ag$pred_upr  <- NA_real_
              
              y_min_ag <- min(df_ag$rate_per_100k, na.rm = TRUE)
              y_max_ag <- max(df_ag$rate_per_100k, na.rm = TRUE)
            }
            
            ag_id  <- gsub("[^A-Za-z0-9]+", "_", ag)
            png_ag <- file.path(out_dir_report,
                                paste0("trend_", code_id, "_", ag_id, ".png"))
            
            png(png_ag, width = 800, height = 600)
            par(mar = c(5, 5, 5, 2))
            
            # Observed rates
            plot(
              df_ag$year_mid, df_ag$rate_per_100k,
              type = "b", pch = 16,
              xlab = "Year",
              ylab = "Cases per 100,000",
              main = paste0(cd, " - ", substr(desc, 1, 60),
                            " (", ag, ")"),
              ylim = c(y_min_ag, y_max_ag)
            )
            
            # Age-specific predicted trend + 95% prediction interval, if available
            if (any(!is.na(df_ag$pred_rate))) {
              lines(df_ag$year_mid, df_ag$pred_rate, lty = 2)
              lines(df_ag$year_mid, df_ag$pred_lwr,  lty = 3)
              lines(df_ag$year_mid, df_ag$pred_upr,  lty = 3)
              
              legend(
                "topleft",
                legend = c("Observed rate", "Predicted rate", "95% PI"),
                lty    = c(1, 2, 3),
                pch    = c(16, NA, NA),
                bty    = "n"
              )
            }
            
            # Labels on observed points
            text(
              df_ag$year_mid, df_ag$rate_per_100k,
              labels = sprintf("%.1f", df_ag$rate_per_100k),
              pos = 3, cex = 0.7
            )
            
            dev.off()
            
            age_plots_html <- c(
              age_plots_html,
              sprintf(
                "<h3>Age group: %s</h3>
                 <img src='%s' alt='Rate trend for %s (%s)' style='max-width:100%%;height:auto;border:1px solid #ccc;' />",
                htmltools::htmlEscape(ag),
                basename(png_ag),
                htmltools::htmlEscape(cd),
                htmltools::htmlEscape(ag)
              )
            )
          }
          
          # Age-specific data table
          df_age_table <- df_code_age %>%
            arrange(age_group, year_mid) %>%
            mutate(
              rate_per_100k = round(rate_per_100k, 2)
            )
          
          age_rows <- apply(df_age_table, 1, function(r) {
            sprintf(
              "<tr>
                 <td>%s</td>
                 <td>%s</td>
                 <td>%s</td>
                 <td>%0.2f</td>
               </tr>\n",
              r[["year_mid"]],
              r[["age_group"]],
              format(as.numeric(r[["cases"]]), big.mark = ",", scientific = FALSE),
              as.numeric(r[["rate_per_100k"]])
            )
          })
          
          age_section_html <- paste0(
            "<h2>Age-specific incidence (per 100,000)</h2>",
            paste(age_plots_html, collapse = "\n"),
            "<h3>Age-specific data</h3>",
            "<table>",
            "<tr>",
            "<th>Year</th>",
            "<th>Age group</th>",
            "<th>Cases</th>",
            "<th>Rate per 100,000</th>",
            "</tr>",
            paste(age_rows, collapse = ""),
            "</table>"
          )
        }
      }
      
      # ----------------- DETAIL HTML (all ages + age breakdown) -----------------
      detail_file <- file.path(out_dir_report, paste0("code_", code_id, ".html"))
      
      df_table <- df_code[, c(
        "year_interval", "year_mid",
        "all_diagnoses", "pred", "pred_lwr", "pred_upr", "is_alert"
      )]
      
      table_rows <- apply(df_table, 1, function(r) {
        sprintf(
          "<tr>
             <td>%s</td>
             <td>%s</td>
             <td>%0.1f</td>
             <td>%0.1f</td>
             <td>%0.1f</td>
             <td>%s</td>
           </tr>\n",
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
        "<h2>Yearly data (all ages)</h2>",
        "<table>",
        "<tr>",
        "<th>Year</th>",
        "<th>Observed diagnoses</th>",
        "<th>Predicted</th>",
        "<th>Lower 95% PI</th>",
        "<th>Upper 95% PI</th>",
        "<th>Anomaly (above upper PI)</th>",
        "</tr>",
        paste(table_rows, collapse = ""),
        "</table>",
        age_section_html,
        "</body></html>"
      )
      
      cat(detail_html, file = detail_file)
      message("Detail HTML written for code ", cd, ": ", detail_file)
    }
  }
}

message("\n=== Analysis complete ===")