# ===================================================================
# STREAMLINED CPS DOWNLOAD, PROCESSING, AND VALIDATION SCRIPT (CSV-ONLY)
# ===================================================================

# ===================================================================
# BEFORE RUNNING: AGK ORIGINAL DATA REQUIRED
# ===================================================================
# This replication requires the original Autor, Goldin, and Katz (2020)
# replication package from openICPSR. Download it here:
#
#   https://www.openicpsr.org/openicpsr/project/120694/version/V2/view
#
# Download the zip file (120694-V2.zip) and place it in this directory
# (the same folder as this script). Script 0 will unzip it automatically
# and copy the necessary files to Data/AGK/.
# ===================================================================

# --- SETUP ---
options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c("tidyverse", "readr", "ipumsr", "haven")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

try(setwd(getSrcDirectory(function(dummy) {dummy})), silent = TRUE)

cat("CPS Data Processing (CSV-only) - Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# --- UNZIP AGK ARCHIVE (ONE-TIME SETUP) ---
setup_agk_files <- function() {
  agk_dir     <- "Data/AGK"
  cps_dir     <- file.path(agk_dir, "CPS")
  zip_file    <- "120694-V2.zip"
  unzip_dir   <- "120694-V2"

  # Check if AGK files are already in place (look for a sentinel file)
  sentinel <- file.path(agk_dir, "marchcells6318.dta")
  if (file.exists(sentinel)) {
    cat("AGK data files already present. Skipping unzip.\n")
    return(invisible())
  }

  # AGK files not found — require the zip
  if (!file.exists(zip_file)) {
    stop(paste0(
      "\nAGK data files not found and 120694-V2.zip is missing.\n",
      "Please download the replication package from:\n",
      "  https://www.openicpsr.org/openicpsr/project/120694/version/V2/view\n",
      "and place 120694-V2.zip in the replication working directory:\n",
      "  ", normalizePath("."), "\n"
    ))
  }

  extract_dir <- "AGK_files"
  cat(sprintf("Unzipping 120694-V2.zip to %s/ ...\n", extract_dir))
  if (!dir.exists(extract_dir)) dir.create(extract_dir)
  unzip(zip_file, exdir = extract_dir)

  # The zip may extract with or without a top-level 120694-V2 subfolder
  # Detect the actual root: look for CPS/cleaned/ under extract_dir or one level down
  candidate_roots <- c(extract_dir, file.path(extract_dir, unzip_dir))
  zip_root <- NULL
  for (r in candidate_roots) {
    if (dir.exists(file.path(r, "CPS", "cleaned"))) { zip_root <- r; break }
  }
  if (is.null(zip_root)) stop(paste0(
    "Could not find CPS/cleaned/ inside the unzipped archive. ",
    "Please check the structure of 120694-V2.zip."
  ))

  # Create destination directories
  if (!dir.exists(cps_dir)) dir.create(cps_dir, recursive = TRUE)
  if (!dir.exists(agk_dir)) dir.create(agk_dir, recursive = TRUE)

  # Copy cleaned CPS files (mar*.dta) to Data/AGK/CPS/
  cps_src <- file.path(zip_root, "CPS", "cleaned")
  cps_files <- list.files(cps_src, pattern = "\\.dta$", full.names = TRUE)
  if (length(cps_files) == 0) stop(paste("No .dta files found in", cps_src))
  cat(sprintf("Copying %d CPS .dta files to Data/AGK/CPS/ ...\n", length(cps_files)))
  file.copy(cps_files, cps_dir, overwrite = TRUE)

  # Copy series files to Data/AGK/
  series_src <- file.path(zip_root, "CPS", "series")
  series_files <- list.files(series_src, pattern = "\\.dta$", full.names = TRUE)
  if (length(series_files) == 0) stop(paste("No .dta files found in", series_src))
  cat(sprintf("Copying %d series .dta files to Data/AGK/ ...\n", length(series_files)))
  file.copy(series_files, agk_dir, overwrite = TRUE)

  # Copy colhs1405.dta (AKK 2008 historical series) to Data/
  colhs_src <- file.path(zip_root, "tables_A1_A2_figure_A1", "colhs1405.dta")
  if (!file.exists(colhs_src)) stop(paste("colhs1405.dta not found in", dirname(colhs_src)))
  cat("Copying colhs1405.dta to Data/ ...\n")
  file.copy(colhs_src, "Data/colhs1405.dta", overwrite = TRUE)

  cat("AGK setup complete.\n")
}

setup_agk_files()

# --- CONVERT EXISTING DTA FILES TO CSV (ONE-TIME) ---
convert_dta_to_csv <- function() {
  # Changed to use unified cleaned directory
  cleaned_dir <- "Data/CPS/cleaned/"
  
  if (!dir.exists(cleaned_dir)) dir.create(cleaned_dir, recursive = TRUE)
  
  # Look for DTA files in potential source directories
  source_dirs <- c("Data/AGK/CPS/", "Data/CPS/downloads/", "Data/CPS/cleaned/")
  dta_files <- c()
  
  for (dir in source_dirs) {
    if (dir.exists(dir)) {
      dta_files <- c(dta_files, list.files(dir, pattern = "\\.dta$", full.names = TRUE))
    }
  }
  
  if (length(dta_files) == 0) {
    cat("No existing .dta files found to convert.\n")
    return()
  }
  
  cat("Converting existing .dta files to CSV (cleaning variable names)...\n")
  for (dta_file in dta_files) {
    csv_file <- file.path(cleaned_dir, gsub("\\.dta$", ".csv", basename(dta_file)))
    
    if (!file.exists(csv_file)) {
      cat("Converting:", basename(dta_file), "->", basename(csv_file), "\n")
      dta_data <- read_dta(dta_file)
      
      # Clean up problematic underscores from variable names
      names(dta_data) <- gsub("^_", "", names(dta_data))
      
      write_csv(dta_data, csv_file)
    }
  }
  cat("DTA to CSV conversion complete.\n")
}

convert_dta_to_csv()

# --- DOWNLOAD CHECK ---
download_cps_data_if_needed <- function(download_dir = "Data/CPS/downloads") {
  existing_files <- list.files(download_dir, pattern = "cps_.*\\.csv(\\.gz)?$", full.names = TRUE)
  
  if (length(existing_files) > 0) {
    cat("Local CPS data found, skipping download.\n")
    return(invisible())
  }
  
  cat("No local data found. Downloading from IPUMS...\n")
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  
  # Set API Key
  # Check for existing API key in environment, prompt user if missing
  api_key <- Sys.getenv("IPUMS_API_KEY")
  
  if (api_key == "") {
    cat("\nIPUMS API key not found in environment.\n")
    api_key <- readline(prompt = "Please enter your IPUMS API key (or press Esc to cancel): ")
    
    if (api_key == "") {
      stop("An IPUMS API key is required to download the 2014-2024 CPS data.")
    }
    
    # Temporarily set it for this session
    Sys.setenv(IPUMS_API_KEY = api_key)
  }
  
  samples <- paste0("cps", 2014:2024, "_03s")
  variables_needed <- c(
    "YEAR", "STATEFIP", "ASECWT", "EMPSTAT", "WKSWORK1", "AGE", "SEX", 
    "RACE", "EDUC", "AHRSWORKT", "UHRSWORKLY", "FULLPART", "UH_CLSLYR_A4", 
    "INDLY", "OCCLY", "UH_RNOWRK_A4", "INCLONGJ", "OINCWAGE", "SRCEARN"
  )
  
  cps_extract <- define_extract_micro(
    collection = "cps",
    description = "CPS processing script (2014-2024)",
    samples = samples,
    variables = variables_needed,
    data_quality_flags = TRUE,
    data_format = "csv"
  )
  
  submitted_extract <- submit_extract(cps_extract)
  ready_extract <- wait_for_extract(submitted_extract)
  download_extract(ready_extract, download_dir = download_dir, overwrite = TRUE)
  cat("Download complete.\n")
}

download_cps_data_if_needed()

# --- LOAD DATA ---
cat("Loading data...\n")
cps_files <- list.files("Data/CPS/downloads", pattern = "cps_.*\\.csv(\\.gz)?$", full.names = TRUE)
cps_file <- tail(cps_files, 1)

if (file.exists(gsub("\\.csv(\\.gz)?$", ".xml", cps_file))) {
  ddi_file <- gsub("\\.csv(\\.gz)?$", ".xml", cps_file)
  cps_data <- read_ipums_micro(ddi_file)
} else {
  cps_data <- read_csv(cps_file)
}

# Load deflators
cpi_data <- read_csv("Data/CPIAUCSL.csv") %>%
  mutate(year = as.numeric(format(as.Date(observation_date), "%Y"))) %>%
  group_by(year) %>%
  summarise(cpi_2012 = mean(CPIAUCSL, na.rm = TRUE), .groups = 'drop')

pce_data <- read_csv("Data/PCEPI.csv") %>%
  mutate(year = as.numeric(format(as.Date(observation_date), "%Y"))) %>%
  group_by(year) %>%
  summarise(gdpdef_2017 = mean(PCEPI, na.rm = TRUE), .groups = 'drop')

# --- HELPER FUNCTIONS ---
calculate_experience_correct <- function(educ_codes, age, race, female) {
  # Map IPUMS EDUC codes to grdatn values
  grdatn <- case_when(
    is.na(educ_codes) ~ NA_real_,
    educ_codes <= 30 ~ 34, educ_codes == 40 ~ 35, educ_codes == 50 ~ 36,
    educ_codes == 60 ~ 37, educ_codes == 71 ~ 38, educ_codes == 73 ~ 39,
    educ_codes == 81 ~ 40, educ_codes == 91 ~ 41, educ_codes == 92 ~ 42,
    educ_codes == 111 ~ 43, educ_codes == 123 ~ 44, educ_codes == 124 ~ 45,
    educ_codes == 125 ~ 46, TRUE ~ 39
  )
  
  educomp <- rep(NA_real_, length(grdatn))
  race_orig <- case_when(race == 100 ~ 1, race == 200 ~ 2, TRUE ~ 3)
  
  # Apply race/gender-specific educomp mapping (condensed)
  coef_matrix <- list(
    # [race, female] -> coefficients for grdatn 31/0, 32, 33/34, 35, 36, 37, 38, 39, 40, 41/42, 43, 44, 45, 46
    `1_0` = c(0.32, 3.19, 7.24, 8.97, 9.92, 10.86, 11.58, 11.99, 13.48, 14.23, 16.17, 17.68, 17.71, 17.83),
    `1_1` = c(0.62, 3.15, 7.23, 8.99, 9.95, 10.87, 11.73, 12.00, 13.35, 14.22, 16.15, 17.64, 17.00, 17.76),
    `2_0` = c(0.92, 3.28, 7.04, 9.02, 9.91, 10.90, 11.41, 11.98, 13.57, 14.33, 16.13, 17.51, 17.83, 18.00),
    `2_1` = c(0.00, 2.90, 7.03, 9.05, 9.99, 10.85, 11.64, 12.00, 13.43, 14.33, 16.04, 17.69, 17.40, 18.00),
    `3_0` = c(0.62, 3.24, 7.14, 9.00, 9.92, 10.88, 11.50, 11.99, 13.53, 14.28, 16.15, 17.60, 17.77, 17.92),
    `3_1` = c(0.31, 3.03, 7.13, 9.02, 9.97, 10.86, 11.69, 12.00, 13.47, 14.28, 16.10, 17.67, 17.20, 17.88)
  )
  
  grd_levels <- c(31, 32, 33, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46)
  
  for (r in 1:3) {
    for (f in 0:1) {
      mask <- (race_orig == r & female == f)
      coefs <- coef_matrix[[paste0(r, "_", f)]]
      for (i in seq_along(grd_levels)) {
        if (i == 3) {
          educomp[mask & (grdatn == 33 | grdatn == 34)] <- coefs[i]
        } else if (i == 10) {
          educomp[mask & (grdatn == 41 | grdatn == 42)] <- coefs[i]
        } else if (i == 1) {
          educomp[mask & (grdatn == 31 | grdatn == 0)] <- coefs[i]
        } else {
          educomp[mask & grdatn == grd_levels[i]] <- coefs[i]
        }
      }
    }
  }
  
  exp <- pmax(pmin(age - educomp - 7, age - 17), 0)
  return(list(educomp = educomp, exp = exp, grdatn = grdatn))
}

get_topcode_values <- function(year) {
  maxer <- case_when(
    year == 2014 ~ 250000, year == 2015 ~ 280000,
    year >= 2016 & year <= 2021 ~ 300000, year >= 2022 ~ 400000, TRUE ~ 300000
  )
  maxwg <- case_when(
    year == 2014 ~ 46000, year == 2015 ~ 56000,
    year >= 2016 & year <= 2021 ~ 56000, year >= 2022 ~ 75000, TRUE ~ 56000
  )
  return(list(maxer = maxer, maxwg = maxwg))
}

clean_cps_year_fixed <- function(df, year_val) {
  topcodes <- get_topcode_values(year_val)
  
  # Rename quality flags if they exist
  if ("QINCLONGJ" %in% names(df)) df <- df %>% rename(QINCLONG = QINCLONGJ)
  if ("QOINCWAGE" %in% names(df)) df <- df %>% rename(QOINCWAGE = QOINCWAGE)
  
  df_clean <- df %>%
    filter(EMPSTAT != 1, WKSWORK1 >= 1 & WKSWORK1 <= 52, AGE >= 17) %>%
    mutate(
      year = year_val - 1, female = as.integer(SEX == 2),
      white = as.integer(RACE == 100), black = as.integer(RACE == 200), 
      other = as.integer(!RACE %in% c(100, 200)), age = pmin(AGE, 90), agely = age - 1,
      wkslyr = WKSWORK1, hours = pmin(coalesce(AHRSWORKT, 0), 98),
      hrslyr = pmin(coalesce(UHRSWORKLY, 0), 98), ftpt = FULLPART,
      # ftpt==3 ("hours vary") included in fulltime: matches AGK clean_march-2014-2018.do line 96
      fulltime = as.integer(ftpt == 1 | ftpt == 3), fullyear = as.integer(wkslyr >= 40 & wkslyr <= 52),
      clslyr = UH_CLSLYR_A4, wageworker = as.integer(clslyr >= 1 & clslyr <= 4),
      selfemp = as.integer(clslyr >= 5 & clslyr <= 6), indlyr = INDLY, occ = OCCLY,
      phh = as.integer(indlyr == 761), pyrsn = UH_RNOWRK_A4,
      studently = as.integer(pyrsn == 4 & wkslyr <= 49),
      incer1_raw = ifelse(INCLONGJ == 99999999, 0, INCLONGJ),
      incwg1_raw = ifelse(OINCWAGE == 99999999, 0, OINCWAGE),
      incer1 = case_when(incer1_raw >= topcodes$maxer ~ topcodes$maxer * 1.5, TRUE ~ incer1_raw),
      incwg1 = case_when(incwg1_raw >= topcodes$maxwg ~ topcodes$maxwg * 1.5, TRUE ~ incwg1_raw),
      ernsrc = SRCEARN, allocated = as.integer((QINCLONG == 1 | QOINCWAGE == 1) & ernsrc == 1),
      # 2014 weight halved: AGK adjusts for the 2014 CPS redesign which doubled the sample
      # (clean_march-2014-2018.do: "if year==2014 { replace wgt=wgt/2 }")
      wgt = case_when(year_val == 2014 ~ ASECWT / 100 / 2, TRUE ~ ASECWT / 100),
      state = STATEFIP
    )

  # Calculate education and experience
  edu_results <- calculate_experience_correct(df_clean$EDUC, df_clean$age, df_clean$RACE, df_clean$female)
  df_clean$educomp <- edu_results$educomp
  df_clean$exp <- edu_results$exp
  df_clean$grdatn <- edu_results$grdatn

  df_clean <- df_clean %>%
    mutate(
      school = case_when(
        EDUC <= 60 ~ 1L, EDUC == 71 | EDUC == 73 ~ 2L, EDUC >= 81 & EDUC <= 92 ~ 3L,
        EDUC == 111 ~ 4L, EDUC >= 123 & EDUC <= 125 ~ 5L
      ),
      grdhi = grdatn, grdcom = 1, wkslyr_orig = wkslyr, incfr1 = 0, incse1 = 0
    ) %>%
    filter(wageworker == 1 | selfemp == 1) %>%
    mutate(
      tcwkwg = as.integer(((incer1 + incwg1) / wkslyr) > ((topcodes$maxer + topcodes$maxwg) * 1.5 / 40)),
      tchrwg = as.integer(((incer1 + incwg1) / (hrslyr * wkslyr)) > ((topcodes$maxer + topcodes$maxwg) * 1.5 / 1400)),
      maxer = topcodes$maxer,
      maxwg = topcodes$maxwg,
      wgt_wks = wgt * wkslyr, wgt_hrs = wgt * wkslyr * hrslyr, wgt_hrs_ft = wgt * hrslyr, hrslwk = hours
    )
  
  return(df_clean)
}

# --- PROCESS DATA ---
cat("Processing and cleaning data...\n")
years_to_process <- sort(unique(cps_data$YEAR))
cleaned_data_list <- map(years_to_process, function(yr) {
  year_data <- cps_data %>% filter(YEAR == yr)
  clean_cps_year_fixed(year_data, yr)
})

all_cleaned <- bind_rows(cleaned_data_list) %>%
  left_join(cpi_data, by = "year") %>%
  left_join(pce_data, by = "year") %>%
  rename(cpi = cpi_2012, gdp = gdpdef_2017) %>%
  mutate(
    winc_ws = case_when(wageworker == 1 & (incer1 + incwg1) > 0 ~ (incer1 + incwg1) / wkslyr, TRUE ~ NA_real_),
    hinc_ws = case_when(wageworker == 1 & (incer1 + incwg1) > 0 & hrslyr > 0 ~ winc_ws / hrslyr, TRUE ~ NA_real_),
    # Bottom-coding filters: winc_ws * gdp converts nominal wages to a common
    # deflator base for comparison against fixed thresholds. The gdp variable here
    # is the raw PCEPI level (not yet rebased to 2008=1), so multiplying scales
    # wages into index units. This matches the AGK Stata convention in
    # clean_march-2014-2018.do where the same arithmetic is used with the raw
    # deflator. The actual real-wage deflation (dividing by the rebased deflator)
    # happens in 1_Demographic_cells_pt1.R after rebasing gdp to 2008=1.
    bcwkwg = as.integer(!is.na(winc_ws) & (winc_ws * gdp) < (40 * (109.031 / 53.615))),
    bchrwg = as.integer(!is.na(hinc_ws) & (winc_ws * gdp / hrslyr) < (1 * (109.031 / 53.615))),
    bcwkwgkm = as.integer(!is.na(winc_ws) & (winc_ws * gdp) < (67 * (109.031 / 53.615))),
    bchrwgkm = as.integer(!is.na(hinc_ws) & (winc_ws * gdp / hrslyr) < (1.675 * (109.031 / 53.615)))
  )

cat("Windsorizing hourly wages...\n")
all_cleaned <- all_cleaned %>%
  mutate(
    hinc_ws = ifelse(
      tchrwg == 1 & !is.na(tchrwg),
      ((maxer + maxwg) * 1.5 / 1400),
      hinc_ws
    )
  )


# --- CONVERT TO CSV ---
cat("Converting to CSV format...\n")
output_dir <- "Data/CPS/cleaned/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

available_years <- sort(unique(all_cleaned$year))
new_files_created <- character()

for (year in available_years) {
  if (year >= 2013 && year <= 2023) {
    year_data <- all_cleaned %>% 
      filter(year == !!year, agely >= 16 & agely <= 64, exp >= 0 & exp <= 48, !is.na(winc_ws), winc_ws > 0)
    
    if (nrow(year_data) == 0) next
    
    survey_year <- year + 1
    y <- survey_year - 1900
    suffix <- if(survey_year >= 2014 && survey_year <= 2018) "_new" else ""
    
    if (y > 60 & y < 100) {
      file_name <- paste0("mar", sprintf("%02d", y), suffix, ".csv")
    } else if (y >= 100 & y < 110) {
      file_name <- paste0("mar0", sprintf("%01d", y-100), suffix, ".csv")
    } else if (y >= 110) {
      file_name <- paste0("mar", sprintf("%02d", y-100), suffix, ".csv")
    }
    
    output_path <- file.path(output_dir, file_name)
    write_csv(year_data, output_path)
    new_files_created <- c(new_files_created, output_path)
  }
}

# --- COMPARE TO ORIGINAL ---
cat("Comparing to original files...\n")

# Year mapping: mar14.csv = 2013 income year, mar15.csv = 2014 income year, etc.
year_mapping <- data.frame(
  file_yr = 14:18,
  income_yr = 2013:2017
)

# Initialize comparison results
all_comparisons <- data.frame()

# Changed to use unified cleaned directory
cleaned_dir <- "Data/CPS/cleaned/"

# Compare each year individually (avoid bind_rows issues)
for (i in 1:nrow(year_mapping)) {
  file_yr <- year_mapping$file_yr[i]
  income_yr <- year_mapping$income_yr[i]
  
  file_path <- file.path(cleaned_dir, paste0("mar", sprintf("%02d", file_yr), ".csv"))
  
  if (file.exists(file_path)) {
    cat("\n=== COMPARING YEAR", income_yr, "(mar", sprintf("%02d", file_yr), ".csv) ===\n", sep = "")
    
    # Load this year's original data
    original_year_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Filter our data for this year
    our_year_data <- all_cleaned %>% filter(year == income_yr)
    
    # Sample size comparison
    n_our <- nrow(our_year_data)
    n_original <- nrow(original_year_data)
    difference <- n_our - n_original
    pct_diff <- if(n_original > 0) round(100 * difference / n_original, 2) else NA
    
    cat("Sample sizes - Our:", n_our, "Original:", n_original, 
        "Difference:", difference, "(%", pct_diff, ")\n")
    
    # Store for summary table
    all_comparisons <- rbind(all_comparisons, 
                             data.frame(year = income_yr, n_our = n_our, 
                                        n_original = n_original, difference = difference, 
                                        pct_diff = pct_diff))
    
    # Key variable comparisons for this year only
    key_vars <- list(
      list("female", "female", "Female indicator"),
      list("white", "white", "White indicator"), 
      list("black", "black", "Black indicator"),
      list("fulltime", "fulltime", "Full-time worker"),
      list("wageworker", "wageworker", "Wage worker"),
      list("school", "school", "Education categories"),
      list("wkslyr", "wkslyr", "Weeks worked last year"),
      list("hrslyr", "hrslyr", "Hours per week last year"),
      list("exp", "exp", "Experience")
    )
    
    for (var_info in key_vars) {
      our_var <- var_info[[1]]
      orig_var <- var_info[[2]]
      desc <- var_info[[3]]
      
      if (our_var %in% names(our_year_data) && orig_var %in% names(original_year_data)) {
        our_vals <- our_year_data[[our_var]]
        orig_vals <- original_year_data[[orig_var]]
        
        # Quick comparison - just means for continuous, correlation for key vars
        if (is.numeric(our_vals) && is.numeric(orig_vals)) {
          our_mean <- round(mean(our_vals, na.rm = TRUE), 4)
          orig_mean <- round(mean(orig_vals, na.rm = TRUE), 4)
          
          # Calculate correlation if sufficient variation
          if (length(unique(our_vals[!is.na(our_vals)])) > 5 && 
              length(unique(orig_vals[!is.na(orig_vals)])) > 5) {
            # Truncate to the shorter vector for correlation. This assumes rows
            # are aligned by position; if missing rows appear mid-file (not just
            # at the tail), position-based alignment could be misleading. This is
            # a quick diagnostic check only — it does not affect any results in
            # the paper.
            min_length <- min(length(our_vals), length(orig_vals))
            corr <- cor(our_vals[1:min_length], orig_vals[1:min_length], use = "complete.obs")
            cat(" ", desc, "- Our mean:", our_mean, "Orig mean:", orig_mean, 
                "Corr:", round(corr, 4), "\n")
          } else {
            cat(" ", desc, "- Our mean:", our_mean, "Orig mean:", orig_mean, "\n")
          }
        }
      }
    }
    
  } else {
    cat("File not found:", file_path, "\n")
  }
}

# Summary table
cat("\n=== OVERALL SAMPLE SIZE COMPARISON ===\n")
print(all_comparisons)

# Check if any years have perfect matches
perfect_matches <- all_comparisons$difference == 0
if (any(perfect_matches)) {
  cat("\nYears with perfect sample size matches:", 
      paste(all_comparisons$year[perfect_matches], collapse = ", "), "\n")
}

# Check for problematic years
problem_years <- abs(all_comparisons$pct_diff) > 5 & !is.na(all_comparisons$pct_diff)
if (any(problem_years)) {
  cat("Years with >5% sample size differences:", 
      paste(all_comparisons$year[problem_years], collapse = ", "), "\n")
}

cat("\n=== SCRIPT COMPLETED ===\n")
cat("All CSV files have been saved to: Data/CPS/cleaned/\n")
cat("Files created:", length(new_files_created), "\n")
if (length(new_files_created) > 0) {
  cat("File list:\n")
  for (file in new_files_created) {
    cat(" -", basename(file), "\n")
  }
}