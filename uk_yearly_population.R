# Packages -------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr) 

# ----------------------------------------------------------------------
# 1. Download the projections zip into data/zip
# ----------------------------------------------------------------------
zip_dir  <- file.path("data", "zip")
proj_dir <- file.path("data", "population_projections")
out_dir  <- file.path("data", "population")

if (!dir.exists(zip_dir))  dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(proj_dir)) dir.create(proj_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir))  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

zip_url  <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/z1zippedpopulationprojectionsdatafilesuk/2022based/uk.zip"
zip_path <- file.path(zip_dir, "uk.zip")

if (!file.exists(zip_path)) {
  message("Downloading population projections zip...")
  download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)
} else {
  message("Zip already exists, skipping download: ", zip_path)
}

# ----------------------------------------------------------------------
# 2. Extract into data/population_projections
# ----------------------------------------------------------------------
message("Unzipping to: ", proj_dir)
unzip(zip_path, exdir = proj_dir)

# ----------------------------------------------------------------------
# 3. Read uk_lll_machine_readable.xlsx and export 2023–2025
# ----------------------------------------------------------------------
xlsx_path <- file.path(proj_dir, "uk", "uk_lll_machine_readable.xlsx")
if (!file.exists(xlsx_path)) {
  stop("Could not find ", xlsx_path, " after unzipping. Check the zip contents.")
}

message("Reading Population sheet from: ", xlsx_path)
pop <- read_excel(xlsx_path, sheet = "Population")

# We want year, sex, age, population for years 2023–2025
years_of_interest <- 2023:2025
year_cols <- as.character(years_of_interest)

# Check that the expected year columns exist
missing_cols <- setdiff(year_cols, colnames(pop))
if (length(missing_cols) > 0) {
  stop("Year columns not found in Population sheet: ",
       paste(missing_cols, collapse = ", "))
}

pop_long <- pop %>%
  select(Sex, Age, all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(year),
    # keep Age as-is (character) because of values like "90 and over"
    sex  = as.character(Sex),
    age  = as.character(Age)
  ) %>%
  select(year, sex, age, population) %>%
  arrange(year, sex, age)

out_csv <- file.path(out_dir, "2023_2025.csv")
write.csv(pop_long, out_csv, row.names = FALSE)

message("Written population data for 2023–2025 to: ", out_csv)

# ----------------------------------------------------------------------
# 4. Download the observed data file into data/zip
# ----------------------------------------------------------------------
zip_dir  <- file.path("data", "zip")
pop_dir  <- file.path("data", "population")

if (!dir.exists(zip_dir)) dir.create(zip_dir, recursive = TRUE)
if (!dir.exists(pop_dir)) dir.create(pop_dir, recursive = TRUE)

url_est <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales/ukpopulationestimates1838to2023editionofthisdataset/ukpopulationestimates18382023.xlsx"
xlsx_path <- file.path(zip_dir, "ukpopulation_estimates.xlsx")

if (!file.exists(xlsx_path)) {
  message("Downloading UK population estimates (1838–2023)...")
  download.file(url_est, destfile = xlsx_path, mode = "wb", quiet = TRUE)
} else {
  message("File already exists, skipping download: ", xlsx_path)
}

# ----------------------------------------------------------------------
# 5. Read Table 3 (mid-year population by single year of age and sex)
# ----------------------------------------------------------------------
sheet_name <- "Table 3"

# First read without column names because Table 3 has a pre-header
raw <- read_excel(xlsx_path, sheet = sheet_name, col_names = FALSE)

# ----------------------------------------------------------------------
# Identify the header row
# The header row contains "Age" and "Sex"
# ----------------------------------------------------------------------
header_row <- which(raw[[1]] == "Age")[1]

if (is.na(header_row)) stop("Could not find the header row with 'Age'.")

# Extract header
headers <- as.character(raw[header_row, ])
headers <- make.names(headers, unique = TRUE)

# Remove rows above header
df <- raw[(header_row + 1):nrow(raw), ]
colnames(df) <- headers

# Remove blank rows
df <- df %>% filter(!is.na(Age))

# ----------------------------------------------------------------------
# Keep only columns for: Age, Sex, and Mid-2015 … Mid-2022
# Column names are like "Mid.2022", "Mid.2021" etc
# ----------------------------------------------------------------------
needed_years <- 2015:2022
year_cols <- paste0("Mid.", needed_years)

missing <- setdiff(year_cols, colnames(df))
if (length(missing) > 0) {
  stop("Missing expected year columns in Table 3: ",
       paste(missing, collapse = ", "))
}

df_sub <- df %>% select(Age, Sex, all_of(year_cols))

# ----------------------------------------------------------------------
# Convert to long format: year, sex, age, population
# ----------------------------------------------------------------------
df_long <- df_sub %>%
  pivot_longer(
    cols = starts_with("Mid."),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    # "Mid.2022" → 2022
    year = as.integer(str_replace(year, "Mid\\.", "")),
    sex  = as.character(Sex),
    age  = as.character(Age),
    population = as.numeric(population)
  ) %>%
  select(year, sex, age, population) %>%
  arrange(year, sex, age)

# ----------------------------------------------------------------------
# 6. Output to data/population/2015_2022.csv
# ----------------------------------------------------------------------
out_csv <- file.path(pop_dir, "2015_2022.csv")
write.csv(df_long, out_csv, row.names = FALSE)

message("Written: ", out_csv)

# ----------------------------------------------------------------------
# 7. Merge 2015–2022 & 2023–2025 into 2015_2025.csv
#    - keep only Males & Females
#    - drop "All Ages"
#    - collapse all ages >= 90 (and range bands) into "90+"
# ----------------------------------------------------------------------

pop_dir <- file.path("data", "population")

pop_2015_2022 <- read.csv(
  file.path(pop_dir, "2015_2022.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pop_2023_2025 <- read.csv(
  file.path(pop_dir, "2023_2025.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Vectorised age normalisation based on observed age_values
normalize_age <- function(age) {
  a <- trimws(as.character(age))
  res <- a
  
  # Drop aggregate / empty
  res[a %in% c("All Ages", "")] <- NA_character_
  
  # Ranges and pre-grouped 90+
  res[a %in% c("105 - 109", "110 and over", "90+")] <- "90+"
  
  # Plain numeric ages -> collapse >= 90 to "90+"
  is_num <- grepl("^[0-9]+$", a)
  if (any(is_num)) {
    nums <- suppressWarnings(as.integer(a[is_num]))
    res[is_num] <- ifelse(nums >= 90, "90+", as.character(nums))
  }
  
  res
}

combined <- bind_rows(pop_2015_2022, pop_2023_2025) %>%
  mutate(
    year = as.integer(year),
    sex  = as.character(sex),
    age  = normalize_age(age)
  ) %>%
  # keep only Males / Females, drop NA ages (e.g. All Ages)
  filter(
    sex %in% c("Males", "Females"),
    !is.na(age),
    year >= 2015, year <= 2025
  ) %>%
  # group in case multiple source rows now share the same "90+"
  group_by(year, sex, age) %>%
  summarise(
    population = sum(as.numeric(population), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # sort by year, sex, then numeric age (with 90+ at the end)
  mutate(
    age_order = ifelse(age == "90+", 90L, as.integer(age))
  ) %>%
  arrange(year, sex, age_order) %>%
  select(year, sex, age, population)

out_2015_2025 <- file.path(pop_dir, "2015_2025.csv")
write.csv(combined, out_2015_2025, row.names = FALSE)

message("Written merged population data for 2015–2025 to: ", out_2015_2025)
