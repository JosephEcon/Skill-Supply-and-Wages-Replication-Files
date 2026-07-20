## Focused IPUMS download + Francis pipeline runner
options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c("tidyverse", "readr", "ipumsr", "haven")
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))

if (Sys.getenv("IPUMS_API_KEY") == "") stop("Set the IPUMS_API_KEY environment variable to your IPUMS API key (see README.md)")

download_dir <- "Data/CPS/downloads"
if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

# Check if data already downloaded
existing <- list.files(download_dir, pattern = "cps_.*\\.csv(\\.gz)?$", full.names = TRUE)
if (length(existing) > 0) {
  cat("CPS data already downloaded:", existing, "\n")
  cat("Skipping download.\n")
} else {
  cat("Submitting fresh IPUMS CPS extract...\n")

  cps_extract <- define_extract_micro(
    collection = "cps",
    description = "CPS March ASEC 2014-2024 for Francis replication",
    samples = paste0("cps", 2014:2024, "_03s"),
    variables = c("YEAR", "STATEFIP", "ASECWT", "EMPSTAT", "WKSWORK1", "AGE", "SEX",
                   "RACE", "EDUC", "AHRSWORKT", "UHRSWORKLY", "FULLPART", "UH_CLSLYR_A4",
                   "INDLY", "OCCLY", "UH_RNOWRK_A4", "INCLONGJ", "OINCWAGE", "SRCEARN"),
    data_quality_flags = TRUE,
    data_format = "csv"
  )

  submitted <- submit_extract(cps_extract)
  cat("Extract submitted:", submitted$number, "\n")
  cat("Waiting for completion (this may take 10-60 minutes)...\n")
  flush.console()

  ready <- wait_for_extract(submitted, verbose = TRUE)
  cat("Extract ready! Downloading...\n")
  flush.console()

  download_extract(ready, download_dir = download_dir, overwrite = TRUE)
  cat("Download complete.\n")
}

cat("\nNow running full pipeline from script 0...\n")
flush.console()
source("0_CPS_data_2014_2024.R", local = new.env())
cat("Script 0 done.\n")

for (i in 1:10) {
  script <- sprintf("%d_%s", i, c(
    "Demographic_cells_pt1", "Demographic_cells_pt2", "Efficiency_units",
    "Wage_equations", "Labor_supply_weights", "Wages", "Wage_premiums",
    "Analysis", "Tests", "Comparison_report")[i])
  script <- paste0(script, ".R")
  if (file.exists(script)) {
    cat(">>> Running:", script, "\n")
    flush.console()
    tryCatch(
      source(script, local = new.env()),
      error = function(e) cat("ERROR in", script, ":", e$message, "\n")
    )
    cat(">>> Done:", script, "\n\n")
    flush.console()
  }
}

cat("=== Pipeline complete ===\n")
