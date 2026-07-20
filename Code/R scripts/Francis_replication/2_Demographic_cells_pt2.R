# CPS March Cell Assembly Script - Enhanced with Comparison
# R replication of assembcellsmarch.do

library(dplyr)
library(haven)  # For reading .dta files

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Try different input directories to match Stata structure
INPUT_DIRS <- c("tmp", "Data/series")  # Check tmp first (Stata path), then fallback
OUTPUT_DIR <- "Data/series"
COMPARISON_FILE <- "Data/AGK/marchcells6318.dta"

# Year ranges for different output files
RANGES <- list(
  list(start = 1964, end = 1988, suffix = "6387"),  # 1963-1987 data
  list(start = 1964, end = 2006, suffix = "6305"),  # 1963-2005 data  
  list(start = 1964, end = 2018, suffix = "6318"),  # 1963-2018 data
  list(start = 1964, end = 2024, suffix = "6323")   # 1963-2023 data
)

# ==============================================================================
# MAIN ASSEMBLY FUNCTION
# ==============================================================================

assemble_march_range <- function(start_year, end_year, suffix) {
  
  cat("Assembling", start_year, "-", end_year, "...")
  
  # Find the correct input directory
  INPUT_DIR <- NULL
  for (dir in INPUT_DIRS) {
    test_file <- file.path(dir, paste0("marchcells-", start_year, ".csv"))
    if (file.exists(test_file)) {
      INPUT_DIR <- dir
      break
    }
  }
  
  if (is.null(INPUT_DIR)) {
    stop("Could not find input files. Checked directories: ", paste(INPUT_DIRS, collapse = ", "))
  }
  
  cat("\n  Using input directory:", INPUT_DIR)
  
  # Read all files in range
  datasets <- list()
  years <- start_year:end_year
  
  for (i in seq_along(years)) {
    file_path <- file.path(INPUT_DIR, paste0("marchcells-", years[i], ".csv"))
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      # Fix column name: R converts _merge to X_merge when reading CSV
      if ("X_merge" %in% names(data)) {
        names(data)[names(data) == "X_merge"] <- "_merge"
      }
      datasets[[i]] <- data
    }
  }
  
  # Remove NULL entries (missing files)
  datasets <- datasets[!sapply(datasets, is.null)]
  
  if (length(datasets) == 0) stop("No valid files found")
  
  # Combine datasets
  combined_data <- bind_rows(datasets)
  
  cat("\n  Total rows after combining:", nrow(combined_data))
  
  # Show _merge distribution if it exists (for diagnostic purposes)
  if ("_merge" %in% names(combined_data)) {
    merge_dist <- table(combined_data$`_merge`, useNA = "ifany")
    cat("\n  _merge distribution:", paste(names(merge_dist), "=", merge_dist, collapse = ", "))
  }
  
  # Create log variables
  combined_data <- combined_data %>%
    mutate(
      lnrwinc = ifelse(is.na(rwinc) | rwinc <= 0, NA, log(rwinc)),
      lnrhinc = ifelse(is.na(rhinc) | rhinc <= 0, NA, log(rhinc))
    )
  
  # Save output
  output_file <- file.path(OUTPUT_DIR, paste0("marchcells", suffix, ".csv"))
  write.csv(combined_data, output_file, row.names = FALSE)
  
  cat(" ✓ (", nrow(combined_data), "obs,", 
      min(combined_data$year, na.rm = TRUE), "-", 
      max(combined_data$year, na.rm = TRUE), ")\n")
  
  return(combined_data)
}

# ==============================================================================
# COMPARISON FUNCTION
# ==============================================================================

compare_datasets <- function(new_data, comparison_file) {
  
  cat("\n=================================================================\n")
  cat("COMPARISON WITH REFERENCE FILE\n")
  cat("=================================================================\n")
  
  if (!file.exists(comparison_file)) {
    cat("⚠ Reference file not found:", comparison_file, "\n")
    return(FALSE)
  }
  
  # Read reference data
  cat("Reading reference file:", comparison_file, "\n")
  ref_data <- read_dta(comparison_file)
  
  # Fix column name mapping: R converts _merge to X_merge
  if ("X_merge" %in% names(new_data) && !("_merge" %in% names(new_data))) {
    names(new_data)[names(new_data) == "X_merge"] <- "_merge"
  }
  
  # Basic dimension comparison
  cat("\nDIMENSION COMPARISON:\n")
  cat("New data:      ", nrow(new_data), "rows x", ncol(new_data), "columns\n")
  cat("Reference data:", nrow(ref_data), "rows x", ncol(ref_data), "columns\n")
  
  # Column comparison
  new_cols <- sort(names(new_data))
  ref_cols <- sort(names(ref_data))
  
  cat("\nCOLUMN COMPARISON:\n")
  cat("New data columns:   ", paste(new_cols, collapse = ", "), "\n")
  cat("Reference columns:  ", paste(ref_cols, collapse = ", "), "\n")
  
  missing_in_new <- setdiff(ref_cols, new_cols)
  missing_in_ref <- setdiff(new_cols, ref_cols)
  
  if (length(missing_in_new) > 0) {
    cat("Missing in new:     ", paste(missing_in_new, collapse = ", "), "\n")
  }
  if (length(missing_in_ref) > 0) {
    cat("Missing in ref:     ", paste(missing_in_ref, collapse = ", "), "\n")
  }
  
  # Year distribution comparison
  cat("\nYEAR DISTRIBUTION:\n")
  new_year_counts <- table(new_data$year)
  ref_year_counts <- table(ref_data$year)
  
  cat("New data years:      ", min(names(new_year_counts)), "-", max(names(new_year_counts)), 
      " (", length(new_year_counts), "years)\n")
  cat("Reference years:     ", min(names(ref_year_counts)), "-", max(names(ref_year_counts)), 
      " (", length(ref_year_counts), "years)\n")
  
  # Show sample year counts
  cat("\nSAMPLE YEAR COUNTS:\n")
  common_years <- intersect(names(new_year_counts), names(ref_year_counts))
  if (length(common_years) > 0) {
    sample_years <- head(common_years, 5)
    for (yr in sample_years) {
      cat("Year", yr, ": New =", new_year_counts[yr], ", Ref =", ref_year_counts[yr], "\n")
    }
  }
  
  # Check for potential duplicates or unique identifier issues
  cat("\nDUPLICATE ANALYSIS:\n")
  
  # Check for duplicate rows in new data
  if ("year" %in% new_cols && any(c("age", "educ", "sex") %in% new_cols)) {
    # Try to identify potential grouping variables
    id_vars <- intersect(c("year", "age", "educ", "sex", "race", "metro"), new_cols)
    new_dupes <- new_data %>% 
      group_by(dplyr::across(dplyr::all_of(id_vars))) %>%
      summarise(n = n(), .groups = "drop") %>% 
      filter(n > 1)
    
    cat("Potential duplicates in new data:", nrow(new_dupes), "groups\n")
    
    if (nrow(new_dupes) > 0 && nrow(new_dupes) <= 10) {
      cat("Sample duplicate groups:\n")
      print(head(new_dupes))
    }
  }
  
  # Check _merge column in reference data
  if ("_merge" %in% names(ref_data)) {
    cat("\n_MERGE COLUMN ANALYSIS (Reference file):\n")
    merge_table <- table(ref_data$`_merge`, useNA = "ifany")
    print(merge_table)
    cat("Note: Original Stata code does not filter based on _merge values\n")
  }
  
  if ("_merge" %in% names(new_data)) {
    cat("\n_MERGE COLUMN ANALYSIS (New data):\n")
    merge_table_new <- table(new_data$`_merge`, useNA = "ifany")
    print(merge_table_new)
  }
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create output directory
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("=================================================================\n")
cat("CPS March Cell Assembly Script\n")
cat("=================================================================\n")

start_time <- Sys.time()

# Process each range
results <- list()
for (range in RANGES) {
  results[[range$suffix]] <- assemble_march_range(range$start, range$end, range$suffix)
}

cat("✅ Created", length(results), "output files in", 
    round(difftime(Sys.time(), start_time, units = "mins"), 2), "minutes\n")

# Compare the 6318 dataset with reference file
if ("6318" %in% names(results)) {
  comparison_result <- compare_datasets(results[["6318"]], COMPARISON_FILE)
}

cat("=================================================================\n")