# Assemble March Predicted Wage Data - CSV Version
# R replication of assemb-marchwg-regs-exp.do

library(dplyr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PRED_WG_DIR <- "Data/CPS/predictions"
SERIES_DIR <- "Data/series"
TMP_DIR <- "Data/tmp"
LOG_DIR <- "Data/log"

# Input/output file pairs
# years = survey years (file names); income years = survey years - 1
# 6318: replicates AGK's assemb-marchwg-regs-exp.do (survey 1964-2018 → income 1963-2017)
FILES <- list(
  list(input = "march-lswts-exp-6387.csv", output = "pred-marwg-6488.csv", years = 1964:1988),
  list(input = "march-lswts-exp-6305.csv", output = "pred-marwg-6406.csv", years = 1964:2006),
  list(input = "march-lswts-exp-6318.csv", output = "pred-marwg-6318.csv", years = 1964:2018),
  list(input = "march-lswts-exp-6323.csv", output = "pred-marwg-6424.csv", years = 1964:2024)
)

# ==============================================================================
# MAIN ASSEMBLY FUNCTION
# ==============================================================================

assemble_march_wage_data <- function(input_file, output_file, years) {
  
  cat("Processing", input_file, "...")
  
  # Load and combine predicted wage files
  wage_data_list <- list()
  for (year in years) {
    pred_file <- file.path(PRED_WG_DIR, paste0("predwg-mar", year, ".csv"))
    if (file.exists(pred_file)) {
      wage_data_list[[length(wage_data_list) + 1]] <- read.csv(pred_file, stringsAsFactors = FALSE)
    }
  }
  
  combined_data <- bind_rows(wage_data_list) %>%
    rename(expcat = exp1) %>%
    mutate(expcat = case_when(
      expcat == 5  ~ 1L, expcat == 15 ~ 2L, expcat == 25 ~ 3L,
      expcat == 35 ~ 4L, expcat == 45 ~ 5L, TRUE ~ NA_integer_
    ))
  
  # Load labor supply weights and merge
  lswts_data <- read.csv(file.path(SERIES_DIR, input_file), stringsAsFactors = FALSE)
  
  merged_data <- combined_data %>%
    left_join(lswts_data, by = c("year", "edcat5", "expcat", "female")) %>%
    filter(!is.na(lswt))
  
  # Calculate weights and real wages
  final_data <- merged_data %>%
    group_by(year) %>%
    mutate(normlswt = lswt / sum(lswt, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(edcat5, female, expcat) %>%
    mutate(avlswt = mean(normlswt, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      # Adding log(gdp) converts nominal predicted log wages to real 2008$.
      # gdp is the PCEPI deflator rebased so that 2008 = 1.0 (see
      # 1_Demographic_cells_pt1.R). For years before 2008, gdp < 1 and
      # log(gdp) < 0, so adding it deflates; for years after 2008, gdp > 1
      # and the addition inflates to the 2008 base.  This is algebraically
      # equivalent to subtracting log(1/gdp) and matches the AGK Stata
      # original: "gen rplnhrw = plnhrw + ln(gdp)" in assemb-marchwg-regs-exp.do.
      rplnhrw = plnhrw + log(gdp),
      rplnwkw = plnwkw + log(gdp)
    ) %>%
    dplyr::select(year, female, edcat5, expcat, rplnwkw, rplnhrw, plnwkw, plnhrw,
           gdp, avlswt, normlswt, lswt) %>%
    arrange(year, female, edcat5, expcat)
  
  # Variable meanings:
  # lswt: Labor supply in cell
  # normlswt: Labor supply share in cell/year  
  # avlswt: Average labor supply share in cell over period
  # plnhrw/plnwkw: Predicted ln hourly/weekly wages (nominal)
  # rplnhrw/rplnwkw: Real predicted ln hourly/weekly wages
  # female: 1=F, 0=M; edcat5: Education cats (1-5); expcat: Experience cats (1-5)
  
  # Save outputs
  write.csv(final_data, file.path(TMP_DIR, output_file), row.names = FALSE)
  write.csv(final_data, file.path(SERIES_DIR, output_file), row.names = FALSE)
  
  year_range <- paste(min(years), max(years), sep = "-")
  cat(" ✓ (", nrow(final_data), "obs,", year_range, ")\n")
  
  return(final_data)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create directories
for (dir in c(TMP_DIR, SERIES_DIR, LOG_DIR)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

cat("=================================================================\n")
cat("Assemble March Predicted Wage Data\n")
cat("=================================================================\n")

start_time <- Sys.time()

# Process each file pair
results <- list()
for (file_pair in FILES) {
  results[[file_pair$output]] <- assemble_march_wage_data(
    file_pair$input, file_pair$output, file_pair$years
  )
}

cat("Created", length(results), "datasets in", 
    round(difftime(Sys.time(), start_time, units = "secs"), 2), "seconds\n")
cat("=================================================================\n")