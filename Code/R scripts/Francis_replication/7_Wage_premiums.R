# CPS Wage Premium Calculator - Corrected Version
# R replication of calc-marchwg-byexp.do with fixes

library(tidyverse)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PRED_DIR <- "Data/CPS/predictions"
WEIGHTS_DIR <- "Data/series"
OUTPUT_DIR <- "Data/series"

# Period-specific runs: weights file, output suffix, last income year
RUNS <- list(
  list(suffix = "6387", weights_file = "pred-marwg-6488.csv", end_year = 1987),
  list(suffix = "6305", weights_file = "pred-marwg-6406.csv", end_year = 2005),
  list(suffix = "6318", weights_file = "pred-marwg-6318.csv", end_year = 2017),
  list(suffix = "6323", weights_file = "pred-marwg-6424.csv", end_year = 2023)
)

# ==============================================================================
# LOAD WEIGHT DATA
# ==============================================================================

load_weights <- function(weights_file) {
  weight_path <- file.path(WEIGHTS_DIR, weights_file)

  if (!file.exists(weight_path)) {
    stop(weights_file, " not found in ", WEIGHTS_DIR,
         ". Run 6_Wages.R first to generate it.")
  }

  cat("  Loading weights from:", weights_file, "\n")
  # Use distinct() on cell characteristics only (no year) so that weights
  # apply to all prediction years regardless of the weights file's year range.
  # Joining on year was the root cause of the post-2017 truncation bug.
  weights <- read.csv(weight_path) %>%
    distinct(female, edcat5, expcat, avlswt)

  return(weights)
}

# ==============================================================================
# MAIN CALCULATION FUNCTION - CORRECTED
# ==============================================================================

calculate_wage_premiums <- function(data, gender_filter = "all") {
  
  # Apply gender filter
  if (gender_filter == "m") {
    data <- data %>% filter(female == 0)
  } else if (gender_filter == "f") {
    data <- data %>% filter(female == 1)
  }
  
  # Calculate overall weighted wages - CORRECTED METHOD
  # Weighted averages follow the AGK Stata structure: numerator uses na.rm=TRUE
  # (dropping NA wage cells) while the denominator sums all avlswt for the group.
  # If rplnwkw is NA for a cell, its avlswt still enters the denominator — a minor
  # mismatch that matches the Stata original and has negligible practical impact
  # (avlswt is almost never missing independently of rplnwkw).
  overall_wages <- data %>%
    group_by(year) %>%
    summarise(
      # High school graduates (edcat5 == 2)
      hsgwg = sum(rplnwkw * avlswt * (edcat5 == 2), na.rm = TRUE) /
        sum(avlswt * (edcat5 == 2), na.rm = TRUE),
      
      # College-plus (edcat5 == 4 OR edcat5 == 5)
      clpwg = sum(rplnwkw * avlswt * (edcat5 %in% c(4, 5)), na.rm = TRUE) /
        sum(avlswt * (edcat5 %in% c(4, 5)), na.rm = TRUE),
      
      # College only (edcat5 == 4)
      clgwg = sum(rplnwkw * avlswt * (edcat5 == 4), na.rm = TRUE) / 
        sum(avlswt * (edcat5 == 4), na.rm = TRUE),
      
      # Postgrad only (edcat5 == 5)
      POwg = sum(rplnwkw * avlswt * (edcat5 == 5), na.rm = TRUE) / 
        sum(avlswt * (edcat5 == 5), na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Calculate experience-specific weighted wages - CORRECTED METHOD
  exp_wages <- data %>%
    group_by(year, expcat) %>%
    summarise(
      # High school by experience
      exphsgwg = sum(rplnwkw * avlswt * (edcat5 == 2), na.rm = TRUE) / 
        sum(avlswt * (edcat5 == 2), na.rm = TRUE),
      
      # College-plus by experience
      expclpwg = sum(rplnwkw * avlswt * (edcat5 %in% c(4, 5)), na.rm = TRUE) /
        sum(avlswt * (edcat5 %in% c(4, 5)), na.rm = TRUE),
      
      # College only by experience
      expclgwg = sum(rplnwkw * avlswt * (edcat5 == 4), na.rm = TRUE) / 
        sum(avlswt * (edcat5 == 4), na.rm = TRUE),
      
      # Postgrad by experience
      expPOwg = sum(rplnwkw * avlswt * (edcat5 == 5), na.rm = TRUE) / 
        sum(avlswt * (edcat5 == 5), na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Merge and calculate premiums
  result <- exp_wages %>%
    left_join(overall_wages, by = "year") %>%
    mutate(
      # College-plus vs High School premiums
      clphsg_all = clpwg - hsgwg,
      clphsg_exp = expclpwg - exphsgwg,
      
      # College vs High School premiums  
      clghsg_all = clgwg - hsgwg,
      clghsg_exp = expclgwg - exphsgwg,

      # Postgrad vs College premiums
      POCL_all = POwg - clgwg,
      POCL_exp = expPOwg - expclgwg
    ) %>%
    dplyr::select(year, expcat,
           clphsg_all, clghsg_all, clphsg_exp, clghsg_exp, POCL_all, POCL_exp,
           clpwg, hsgwg, clgwg) %>%
    arrange(year, expcat)
  
  return(result)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create directories
for (dir in c("Data/tmp", OUTPUT_DIR)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

cat("CPS Wage Premium Calculator - Period-Specific Runs\n")
cat("==========================================\n\n")

# Load all prediction data once (covers all years; each run filters to its period)
cat("Loading prediction data...\n")
prediction_files <- list.files(PRED_DIR, pattern = "predwg-mar\\d{4}\\.csv$", full.names = TRUE)
if (length(prediction_files) == 0) stop("No prediction files found in ", PRED_DIR)

all_predictions <- map_dfr(prediction_files, function(file) {
  year <- as.numeric(str_extract(basename(file), "\\d{4}"))
  data <- read.csv(file)
  data$year <- year - 1  # File year is one higher than data year
  return(data)
})

# Prepare base prediction data (common to all runs)
pred_data_base <- all_predictions %>%
  mutate(
    # rplnwkw is set to plnwkw (nominal predicted wages) rather than the real
    # wages from 6_Wages.R. This deviates from the Stata original but has no
    # effect on the wage *premiums*: the deflator cancels in every college-minus-HS
    # difference because both groups use the same price index.
    rplnwkw = plnwkw,
    expcat = case_when(
      exp1 >= 0 & exp1 <= 9 ~ 1L,
      exp1 >= 10 & exp1 <= 19 ~ 2L,
      exp1 >= 20 & exp1 <= 29 ~ 3L,
      exp1 >= 30 & exp1 <= 39 ~ 4L,
      exp1 >= 40 & exp1 <= 49 ~ 5L,
      TRUE ~ NA_integer_
    ),
    edcat5 = as.numeric(edcat5)
  ) %>%
  filter(!is.na(expcat), !is.na(edcat5), !is.na(rplnwkw)) %>%
  dplyr::select(year, female, edcat5, expcat, exp1, rplnwkw)

cat("Loaded predictions for", length(unique(pred_data_base$year)), "income years\n\n")

# Process each period
for (run in RUNS) {
  cat(sprintf("--- Run %s (through %d) ---\n", run$suffix, run$end_year))

  # Load period-specific weights (join on cell characteristics only, not year)
  weights_data <- load_weights(run$weights_file)

  # Filter predictions to this period
  pred_data <- pred_data_base %>%
    filter(year <= run$end_year)

  # Merge with weights on (female, edcat5, expcat) — no year in the key.
  # This avoids silently dropping post-weights-period observations.
  pred_data_weighted <- pred_data %>%
    left_join(weights_data, by = c("female", "edcat5", "expcat")) %>%
    filter(!is.na(avlswt)) %>%
    arrange(year, expcat, female, edcat5)

  cat("  Observations:", nrow(pred_data_weighted),
      "| Years:", paste(range(pred_data_weighted$year), collapse = "-"), "\n")

  # Calculate premiums for each gender and combined
  male_results <- calculate_wage_premiums(pred_data_weighted, "m") %>%
    dplyr::rename_with(~ paste0(.x, "_m"), -c(year, expcat))
  female_results <- calculate_wage_premiums(pred_data_weighted, "f") %>%
    dplyr::rename_with(~ paste0(.x, "_f"), -c(year, expcat))
  combined_results <- calculate_wage_premiums(pred_data_weighted, "mf")

  # Merge and clean
  final_results <- combined_results %>%
    left_join(male_results, by = c("year", "expcat")) %>%
    left_join(female_results, by = c("year", "expcat")) %>%
    arrange(year, expcat) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))

  # Save
  output_file <- file.path(OUTPUT_DIR,
                            paste0("clghsgwg-march-regseries-exp-", run$suffix, ".csv"))
  write.csv(final_results, output_file, row.names = FALSE)
  cat("  Saved:", output_file, "\n\n")
}