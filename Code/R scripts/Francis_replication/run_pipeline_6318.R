# ======================================================================
# Run Francis pipeline for available data only (through 2018 = 6318 series)
# Skips script 0 (IPUMS download) since we have data from AGK archive
# ======================================================================

rm(list = ls())
setwd("C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages/Code/R scripts/Francis_replication")

cat("=================================================================\n")
cat("Running Francis Pipeline (6318 series only - through 2018)\n")
cat("=================================================================\n\n")

total_start <- Sys.time()

# Helper: read script, apply substitutions, evaluate
run_modified <- function(script, replacements) {
  cat(">>> Executing:", script, "\n")
  code <- readLines(script)
  for (r in replacements) {
    code <- sub(r$from, r$to, code, fixed = TRUE)
  }
  tryCatch({
    eval(parse(text = paste(code, collapse = "\n")), envir = new.env())
    cat(">>> Completed:", script, "\n\n")
  }, error = function(e) {
    cat(">>> ERROR in", script, ":", e$message, "\n\n")
  })
}

run_safe <- function(script) {
  cat(">>> Executing:", script, "\n")
  tryCatch({
    source(script, local = new.env())
    cat(">>> Completed:", script, "\n\n")
  }, error = function(e) {
    cat(">>> ERROR in", script, ":", e$message, "\n\n")
  })
}

# --- Check AGK data ---
cat(">>> Checking AGK data setup...\n")
if (file.exists("Data/AGK/marchcells6318.dta")) {
  cat("AGK data already present.\n\n")
} else {
  stop("AGK data not found. Run script 0 first.")
}

# --- Script 1: Demographic cells pt1 (cap at 2018) ---
run_modified("1_Demographic_cells_pt1.R", list(
  list(from = "END_YEAR <- 2024", to = "END_YEAR <- 2018")
))

# --- Script 2: Demographic cells pt2 (remove 6323 range) ---
# Need to: 1) remove comma after 6318 line, 2) remove 6323 line entirely
run_modified("2_Demographic_cells_pt2.R", list(
  list(from = 'suffix = "6318"),  # 1963-2018 data',
       to   = 'suffix = "6318")   # 1963-2018 data (last)'),
  list(from = '  list(start = 1964, end = 2024, suffix = "6323")   # 1963-2023 data',
       to   = '  # SKIPPED: 6323 (no 2019+ data)')
))

# --- Script 3: Efficiency units (remove 6323 and 6305-extended configs) ---
run_modified("3_Efficiency_units.R", list(
  list(from = '  list(suffix = "6318", input = "marchcells6318.csv", output = "effunits-exp-byexp-6318.csv", ',
       to   = '  list(suffix = "6318", input = "marchcells6318.csv", output = "effunits-exp-byexp-6318.csv",'),
  list(from = '       compare_file = "effunits-exp-byexp-6318.dta", years = c(1963, 2017)),',
       to   = '       compare_file = "effunits-exp-byexp-6318.dta", years = c(1963, 2017))'),
  list(from = '  list(suffix = "6323", input = "marchcells6323.csv", output = "effunits-exp-byexp-6323.csv"),',
       to   = '  # SKIPPED: 6323'),
  list(from = '  list(suffix = "6305-extended", input = "marchcells6323.csv", output = "effunits-exp-byexp-6305-extended.csv",',
       to   = '  # SKIPPED: 6305-extended (depends on 6323)'),
  list(from = '       weight_years = c(1963, 2005), analysis_years = c(1963, 2023))',
       to   = '  # (continued skip)')
))

# --- Script 4: Wage equations (cap at 2018) ---
run_modified("4_Wage_equations.R", list(
  list(from = "END_YEAR <- 2024", to = "END_YEAR <- 2018")
))

# --- Script 5: Labor supply weights (remove 6323 config) ---
run_modified("5_Labor_supply_weights.R", list(
  list(from = 'output = "march-lswts-exp-6318.csv", compare = TRUE), # Compare this one (1963-2017)',
       to   = 'output = "march-lswts-exp-6318.csv", compare = TRUE) # Compare this one (last)'),
  list(from = '  list(input = "marchcells6323.csv", output = "march-lswts-exp-6323.csv")',
       to   = '  # SKIPPED: 6323')
))

# --- Script 6: Wages (remove 6323 config) ---
run_modified("6_Wages.R", list(
  list(from = 'output = "pred-marwg-6318.csv", years = 1964:2018),',
       to   = 'output = "pred-marwg-6318.csv", years = 1964:2018)'),
  list(from = '  list(input = "march-lswts-exp-6323.csv", output = "pred-marwg-6424.csv", years = 1964:2024)',
       to   = '  # SKIPPED: 6323 config')
))

# --- Script 7: Wage premiums (remove 6323 config) ---
run_modified("7_Wage_premiums.R", list(
  list(from = 'end_year = 2017),',
       to   = 'end_year = 2017)'),
  list(from = '  list(suffix = "6323", weights_file = "pred-marwg-6424.csv", end_year = 2023)',
       to   = '  # SKIPPED: 6323 config')
))

# --- Script 8: Analysis (remove 6323 config) ---
run_modified("8_Analysis.R", list(
  list(from = 'title_period = "1963 - 2017"),',
       to   = 'title_period = "1963 - 2017")'),
  list(from = '  list(suffix = "6323", end_year = 2023, title_period = "1963 - 2023")',
       to   = '  # SKIPPED: 6323 config')
))

# --- Script 9: Tests ---
run_safe("9_Tests.R")

# --- Script 10: Comparison report ---
run_safe("10_Comparison_report.R")

total_end <- Sys.time()
cat("=================================================================\n")
cat("Pipeline Complete (6318 series)!\n")
cat("Total time:", round(difftime(total_end, total_start, units = "mins"), 2), "minutes\n")
cat("=================================================================\n")

# Check outputs
cat("\nOutput files in Data/series/:\n")
for (f in list.files("Data/series", pattern = "\\.csv$")) cat("  ", f, "\n")
cat("\nOutput files in Output/:\n")
for (f in list.files("Output", pattern = "\\.(csv|png|pdf)$")) cat("  ", f, "\n")
