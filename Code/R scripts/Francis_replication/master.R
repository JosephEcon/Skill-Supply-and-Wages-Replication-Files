# ======================================================================
# MASTER EXECUTION SCRIPT: Katz-Murphy / Autor-Goldin-Katz Replication
# ======================================================================
#
# BEFORE RUNNING: Download the AGK replication package from openICPSR:
#
#   https://www.openicpsr.org/openicpsr/project/120694/version/V2/view
#
# Download 120694-V2.zip and place it in this directory (alongside this
# script). Script 0 will unzip it and copy the required files to Data/AGK/
# automatically. This step only runs once; subsequent runs skip it.
#
# ======================================================================

# Clear the global environment to ensure a clean run
rm(list = ls())

setwd(getSrcDirectory(function(dummy) {dummy}))

# Define the pipeline sequence
scripts <- c(
  "0_CPS_data_2014_2024.R",
  "1_Demographic_cells_pt1.R",
  "2_Demographic_cells_pt2.R",
  "3_Efficiency_units.R",
  "4_Wage_equations.R",
  "5_Labor_supply_weights.R",
  "6_Wages.R",
  "7_Wage_premiums.R",
  "8_Analysis.R",
  "9_Tests.R",
  "10_Comparison_report.R"
)

cat("=================================================================\n")
cat("Starting Full Replication Pipeline\n")
cat("=================================================================\n\n")

total_start_time <- Sys.time()

for (script in scripts) {
  if (file.exists(script)) {
    cat(">>> Executing:", script, "\n")
    script_start_time <- Sys.time()
    
    # Run the script in a new environment to prevent variable bleed
    tryCatch({
      source(script, local = new.env())
      script_end_time <- Sys.time()
      cat("\n>>> Successfully completed:", script, "in", 
          round(difftime(script_end_time, script_start_time, units = "mins"), 2), "minutes\n\n")
      cat("-----------------------------------------------------------------\n\n")
    }, error = function(e) {
      cat("\n>>> ERROR executing", script, ":\n", e$message, "\n")
      stop("Pipeline aborted due to error.")
    })
    
  } else {
    stop(paste("Script not found in the working directory:", script))
  }
}

total_end_time <- Sys.time()
cat("=================================================================\n")
cat("Pipeline Complete!\n")
cat("Total execution time:", round(difftime(total_end_time, total_start_time, units = "mins"), 2), "minutes\n")
cat("=================================================================\n")