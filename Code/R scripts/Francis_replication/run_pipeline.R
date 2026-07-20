# Run full Francis replication pipeline
if (Sys.getenv("IPUMS_API_KEY") == "") stop("Set the IPUMS_API_KEY environment variable to your IPUMS API key (see README.md)")

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

for (s in scripts) {
  cat("\n========================================\n")
  cat("Running:", s, "\n")
  cat("========================================\n")
  tryCatch({
    source(s, local = FALSE)
    cat("COMPLETED:", s, "\n")
  }, error = function(e) {
    cat("ERROR in", s, ":", conditionMessage(e), "\n")
  })
}

cat("\n\nPipeline finished.\n")
