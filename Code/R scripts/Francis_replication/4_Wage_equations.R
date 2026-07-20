# CPS Wage Prediction Script - Fixed Version
# R replication of predict-marchwg-regs-exp.do

library(tidyverse)
library(broom)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SINGLE_YEAR <- NULL  # Set to specific year or NULL for range
START_YEAR <- 1964
END_YEAR <- 2024
DATA_DIR <- "Data/CPS/cleaned"

# ==============================================================================
# MAIN PREDICTION FUNCTION
# ==============================================================================

predict_wages_for_year <- function(year) {
  
  # Determine file path
  y <- year - 1900
  if (y > 60 & y < 100) {
    file_name <- paste0("mar", sprintf("%02d", y), ".csv")
  } else if (y >= 100 & y < 110) {
    file_name <- paste0("mar0", sprintf("%01d", y - 100), ".csv")
  } else {
    file_name <- paste0("mar", sprintf("%02d", y - 100), ".csv")
  }
  
  file_path <- file.path(DATA_DIR, file_name)
  if (!file.exists(file_path)) {
    alt_file <- file.path(DATA_DIR, paste0(tools::file_path_sans_ext(file_name), "_new.csv"))
    if (file.exists(alt_file)) file_path <- alt_file else stop("File not found: ", file_path)
  }
  
  # Load and process data
  march_data <- read.csv(file_path) %>%
    # Wage floor filters (bcwkwgkm/bchrwgkm) applied upstream in script 1: winc_ws
    # is set to NA for bottom-coded observations, so !is.na(winc_ws) implicitly
    # enforces the $40/week and $1.675/hour (1982$) floors. Matches AGK practice.
    filter(agely >= 16 & agely <= 64, exp <= 48, selfemp != 1, !is.na(winc_ws))
  
  # Convert variables to numeric
  numeric_vars <- c("agely", "exp", "female", "fulltime", "fullyear", "selfemp", 
                    "allocated", "wgt", "wkslyr", "hrslyr", "winc_ws", "hinc_ws",
                    "bcwkwgkm", "bchrwgkm", "tcwkwg", "tchrwg", "black", "other",
                    "year", "gdp", "grdhi", "grdcom", "grdatn")
  
  for (var in numeric_vars) {
    if (var %in% names(march_data)) march_data[[var]] <- as.numeric(march_data[[var]])
  }
  
  # Filter allocated observations (applied universally; allocated == 0 for pre-1976 years)
  march_data <- march_data %>% filter(allocated != 1)
  
  # Education categories
  if (year <= 1991) {
    # Handle missing values
    march_data$grdhi[is.na(march_data$grdhi)] <- 0
    march_data$grdcom[is.na(march_data$grdcom)] <- 1
    
    march_data <- march_data %>%
      mutate(
        educomp = pmax(0, grdhi - (grdcom == 2), na.rm = TRUE),
        ed8 = as.integer(educomp <= 8),
        ed9 = as.integer(educomp == 9),
        ed10 = as.integer(educomp == 10),
        ed11 = as.integer(educomp == 11),
        edhsg = as.integer(educomp == 12 & grdcom == 1),
        edsmc = as.integer((educomp >= 13 & educomp <= 15) | (educomp == 12 & grdcom == 2)),
        edclg = as.integer(educomp == 16 | educomp == 17),
        edgtc = as.integer(educomp > 17)
      )
  } else {
    # Handle missing values
    march_data$grdatn[is.na(march_data$grdatn)] <- 0
    
    march_data <- march_data %>%
      mutate(
        ed8 = as.integer(grdatn <= 34),
        ed9 = as.integer(grdatn == 35),
        ed10 = as.integer(grdatn == 36),
        ed11 = as.integer(grdatn == 37),
        edhsg = as.integer(grdatn == 38 | grdatn == 39),
        edsmc = as.integer(grdatn >= 40 & grdatn <= 42),
        edclg = as.integer(grdatn == 43),
        edgtc = as.integer(grdatn >= 44)
      )
  }
  
  # Create variables
  march_data <- march_data %>%
    mutate(
      edhsd = ed8 + ed9 + ed10 + ed11,
      exp1 = exp, exp2 = (exp^2) / 100, exp3 = (exp^3) / 1000, exp4 = (exp^4) / 10000,
      e1edhsd = exp1 * edhsd, e1edsmc = exp1 * edsmc, e1edclg = exp1 * (edclg | edgtc),
      e2edhsd = exp2 * edhsd, e2edsmc = exp2 * edsmc, e2edclg = exp2 * (edclg | edgtc),
      e3edhsd = exp3 * edhsd, e3edsmc = exp3 * edsmc, e3edclg = exp3 * (edclg | edgtc),
      e4edhsd = exp4 * edhsd, e4edsmc = exp4 * edsmc, e4edclg = exp4 * (edclg | edgtc),
      ftfy = fulltime * fullyear,
      pt = if ("pt" %in% names(march_data)) as.numeric(pt) else as.integer(!fulltime),
      lnwinc = log(winc_ws), lnhinc = log(hinc_ws),
      tcwkwg = 0,
      tchrwg = 0,
      bcwkwgkm = if ("bcwkwgkm" %in% names(march_data)) bcwkwgkm else 0,
      bchrwgkm = if ("bchrwgkm" %in% names(march_data)) bchrwgkm else 0
    )
  
  # Regression formulas
  reg_formula <- lnwinc ~ edhsd + edsmc + edclg + edgtc + exp1 + exp2 + exp3 + exp4 +
    e1edhsd + e1edsmc + e1edclg + e2edhsd + e2edsmc + e2edclg +
    e3edhsd + e3edsmc + e3edclg + e4edhsd + e4edsmc + e4edclg + black + other
  
  reg_formula_hrly <- lnhinc ~ edhsd + edsmc + edclg + edgtc + exp1 + exp2 + exp3 + exp4 +
    e1edhsd + e1edsmc + e1edclg + e2edhsd + e2edsmc + e2edclg +
    e3edhsd + e3edsmc + e3edclg + e4edhsd + e4edsmc + e4edclg + pt + black + other
  
  # Weekly wage regressions (full-time, full-year workers only)
  male_data <- march_data %>% filter(ftfy == 1, female == 0, tcwkwg == 0, bcwkwgkm == 0)
  female_data <- march_data %>% filter(ftfy == 1, female == 1, tcwkwg == 0, bcwkwgkm == 0)
  
  male_wkly_reg <- lm(reg_formula, data = male_data, weights = wgt)
  female_wkly_reg <- lm(reg_formula, data = female_data, weights = wgt)

  # Hourly wage regressions (all workers, not just full-time full-year)
  male_hrly_data <- march_data %>% filter(female == 0, bchrwgkm == 0, tchrwg == 0, !is.na(lnhinc))
  female_hrly_data <- march_data %>% filter(female == 1, bchrwgkm == 0, tchrwg == 0, !is.na(lnhinc))
  
  # Check sample sizes and use full data if too small
  if (nrow(male_hrly_data) < 50) {
    male_hrly_data <- march_data %>% filter(female == 0, !is.na(lnhinc))
  }
  if (nrow(female_hrly_data) < 50) {
    female_hrly_data <- march_data %>% filter(female == 1, !is.na(lnhinc))
  }
  
  male_hrly_reg <- lm(reg_formula_hrly, data = male_hrly_data, weights = wgt * wkslyr)
  female_hrly_reg <- lm(reg_formula_hrly, data = female_hrly_data, weights = wgt * wkslyr)
  
  # Generate predictions
  ed_levels <- c("edhsd", "edhsg", "edsmc", "edclg", "edgtc")
  exp_levels <- c(5, 15, 25, 35, 45)
  genders <- c(0, 1)
  
  pred_data <- expand_grid(education = ed_levels, experience = exp_levels, female = genders) %>%
    mutate(
      edhsd = 0, edhsg = 0, edsmc = 0, edclg = 0, edgtc = 0,
      exp1 = experience, exp2 = (experience^2) / 100, 
      exp3 = (experience^3) / 1000, exp4 = (experience^4) / 10000,
      black = 0, other = 0, pt = 0, gdp = march_data$gdp[1], year = march_data$year[1]
    )
  
  # Set education indicators
  for (i in 1:nrow(pred_data)) pred_data[i, pred_data$education[i]] <- 1
  
  # Create interactions
  pred_data <- pred_data %>%
    mutate(
      e1edhsd = exp1 * edhsd, e1edsmc = exp1 * edsmc, e1edclg = exp1 * (edclg | edgtc),
      e2edhsd = exp2 * edhsd, e2edsmc = exp2 * edsmc, e2edclg = exp2 * (edclg | edgtc),
      e3edhsd = exp3 * edhsd, e3edsmc = exp3 * edsmc, e3edclg = exp3 * (edclg | edgtc),
      e4edhsd = exp4 * edhsd, e4edsmc = exp4 * edsmc, e4edclg = exp4 * (edclg | edgtc)
    )
  
  # Generate predictions and create final dataset
  final_data <- pred_data %>%
    mutate(
      plnwkw = ifelse(female == 0, predict(male_wkly_reg, newdata = .), predict(female_wkly_reg, newdata = .)),
      plnhrw = ifelse(female == 0, predict(male_hrly_reg, newdata = .), predict(female_hrly_reg, newdata = .)),
      edcat5 = case_when(edhsd == 1 ~ 1L, edhsg == 1 ~ 2L, edsmc == 1 ~ 3L, edclg == 1 ~ 4L, edgtc == 1 ~ 5L),
      exp1 = experience
      # year and gdp are taken directly from march_data and already reflect the
      # income/earnings year (one year before the survey file year), matching the
      # Stata predict-marchwg-regs-exp.do convention: gen year = survey_year - 1
    ) %>%
    dplyr::select(plnwkw, plnhrw, female, edcat5, exp1, year, gdp) %>%
    arrange(female, edcat5, exp1)
  
  # Save results
  write.csv(final_data, paste0("Data/CPS/predictions/predwg-mar", year, ".csv"), row.names = FALSE)
  
  return(final_data)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create directories
for (dir in c("Data/tmp", "Data/CPS/predictions")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

if (!dir.exists(DATA_DIR)) stop("Data directory not found: ", DATA_DIR)

cat("=================================================================\n")
cat("CPS Wage Prediction Script - Fixed Version\n")
cat("=================================================================\n")

start_time <- Sys.time()

if (!is.null(SINGLE_YEAR)) {
  cat(SINGLE_YEAR)
  predict_wages_for_year(SINGLE_YEAR)
  cat(" ✓\n")
  cat("✅ Complete! Output: Data/CPS/predictions/predwg-mar", SINGLE_YEAR, ".csv\n")
} else {
  years <- START_YEAR:END_YEAR
  cat("Processing", length(years), "years (", START_YEAR, "-", END_YEAR, ")\n")
  
  successful_years <- 0
  
  for (year in years) {
    cat(year)
    result <- tryCatch({
      predict_wages_for_year(year)
      cat(" ✓\n")
      successful_years <- successful_years + 1
    }, error = function(e) {
      cat(" ✗ (", e$message, ")\n")
      NULL
    })
  }
  
  cat("✅ Successfully processed", successful_years, "of", length(years), "years\n")
}

cat("Time:", round(difftime(Sys.time(), start_time, units = "mins"), 2), "minutes\n")
cat("=================================================================")