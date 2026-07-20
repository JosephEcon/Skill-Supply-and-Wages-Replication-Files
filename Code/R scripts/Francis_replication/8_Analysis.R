# Katz-Murphy College/High School Relative Supply and Wage Differential Analysis
# Pure R implementation with CSV inputs and outputs - No plotting

library(dplyr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

INPUT_DIR <- "Data/series"
OUTPUT_DIR <- "Data"

# Time period configurations
PERIODS <- list(
  list(suffix = "6387", end_year = 1987, title_period = "1963 - 1987"),
  list(suffix = "6305", end_year = 2005, title_period = "1963 - 2005"),
  list(suffix = "6318", end_year = 2017, title_period = "1963 - 2017"),
  list(suffix = "6323", end_year = 2023, title_period = "1963 - 2023")
)

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

run_km_analysis <- function(suffix, end_year, title_period) {
  
  cat("Processing", suffix, "...")
  
  # Load and merge data
  wage_data <- read.csv(file.path(INPUT_DIR,
                                  paste0("clghsgwg-march-regseries-exp-", suffix, ".csv"))) %>%
    dplyr::select(year, dplyr::starts_with("clghsg_all"), dplyr::starts_with("clphsg_all"),
           dplyr::starts_with("clpwg"), dplyr::starts_with("hsgwg")) %>%
    distinct(year, .keep_all = TRUE)
  
  supply_data <- read.csv(file.path(INPUT_DIR, paste0("effunits-exp-byexp-", suffix, ".csv"))) %>%
    filter(expcat == 1) %>%
    dplyr::select(-expcat)
  
  merged_data <- merge(wage_data, supply_data, by = "year", all = FALSE) %>%
    filter(year <= end_year)
  
  # Create trend terms.
  # Time trend starts at 1 in 1963 (t = year - 1962). 9_Tests.R uses
  # time = year - 1963 (starting at 0). The shift is absorbed by the
  # intercept and lower-order polynomial terms; fitted values are identical.
  merged_data <- merged_data %>%
    mutate(
      t = year - 1962,
      t2 = t * t,
      t3 = t2 * t
    )
  
  # Regressions
  reg_6387 <- lm(clphsg_all ~ t + eu_lnclg, data = merged_data %>% filter(year < 1988))
  reg_linear <- lm(clphsg_all ~ t + eu_lnclg, data = merged_data)
  reg_quadratic <- lm(clphsg_all ~ t + t2 + eu_lnclg, data = merged_data)
  reg_cubic <- lm(clphsg_all ~ t + t2 + t3 + eu_lnclg, data = merged_data)
  
  # Predictions
  merged_data$gap6387 <- predict(reg_6387, newdata = merged_data)
  merged_data$gap_linear <- predict(reg_linear)
  merged_data$gap_quadratic <- predict(reg_quadratic)
  merged_data$gap_cubic <- predict(reg_cubic)
  
  # Transform for plotting (keeping these variables even though no plots)
  merged_data <- merged_data %>%
    mutate(
      exp_gap_quadratic = 100 * (exp(gap_quadratic) - 1),
      exp_clphsg_all = (exp(clphsg_all) - 1) * 100
    )
  
  # Save datasets
  write.csv(merged_data, file.path(OUTPUT_DIR, paste0("km-plot-", suffix, ".csv")), row.names = FALSE)
  
  subset_data <- merged_data %>% 
    dplyr::select(year, clphsg_all, eu_lnclg, ln_supp_clg, ln_supp_hsg, clpwg, hsgwg)
  
  write.csv(subset_data, file.path(OUTPUT_DIR, paste0("km-cg-rsup-", suffix, ".csv")), row.names = FALSE)
  
  cat(" ✓\n")
  return(merged_data)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Create log
sink(file.path(OUTPUT_DIR, "km-analysis.log"), append = FALSE, split = TRUE)
cat("Katz-Murphy Analysis Log - Started at:", as.character(Sys.time()), "\n\n")

# Process all periods
results <- list()
for (period in PERIODS) {
  results[[period$suffix]] <- run_km_analysis(period$suffix, period$end_year, period$title_period)
}

cat("\n Generated", length(PERIODS) * 2, "files:", 
    "\n- 8 CSV datasets",
    "\nCompleted at:", as.character(Sys.time()), "\n")

sink()