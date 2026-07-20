# March Labor Supply Weights Assembly Script - Enhanced with Comparison
# R replication of assemb-march-lswts-exp.do

library(dplyr)
library(haven)  # For reading .dta files

# ==============================================================================
# CONFIGURATION
# ==============================================================================

INPUT_DIR <- "Data/series"
OUTPUT_DIR <- "Data/series"
COMPARISON_FILE <- "Data/AGK/march-lswts-exp.dta"

# Input/output file pairs
FILES <- list(
  list(input = "marchcells6387.csv", output = "march-lswts-exp-6387.csv"),
  list(input = "marchcells6305.csv", output = "march-lswts-exp-6305.csv"),
  list(input = "marchcells6318.csv", output = "march-lswts-exp-6318.csv", compare = TRUE), # Compare this one (1963-2017)
  list(input = "marchcells6323.csv", output = "march-lswts-exp-6323.csv")
)

# ==============================================================================
# MAIN ASSEMBLY FUNCTION
# ==============================================================================

assemble_lswts_exp <- function(input_file, output_file) {
  
  cat("Processing", input_file, "...")
  
  # Load data
  # Note: we aggregate q_lsweight (= wgt * wkslyr, weeks only), matching AGK's
  # assemb-march-lswts-exp.do line 38. The label in assembcellsmarch.do erroneously
  # describes q_lsweight as "hours x weeks x weight"; the actual computation in
  # prepmarchcell.do line 111 is "gen q_lsweight = wgt*_wkslyr" (weeks only).
  # The hours variable is q_lshrsweight (prepmarchcell.do line 113), which is not
  # used here.
  march_data <- read.csv(file.path(INPUT_DIR, input_file))
  
  # Create experience categories and aggregate
  result <- march_data %>%
    dplyr::select(q_lsweight, exp, edcat5, year, female) %>%
    mutate(
      expcat = case_when(
        exp >= 0 & exp <= 9   ~ 1L,  # 0-9 years (age 5)
        exp >= 10 & exp <= 19 ~ 2L,  # 10-19 years (age 15)
        exp >= 20 & exp <= 29 ~ 3L,  # 20-29 years (age 25)
        exp >= 30 & exp <= 39 ~ 4L,  # 30-39 years (age 35)
        exp >= 40 & exp <= 48 ~ 5L,  # 40-48 years (age 45)
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(!is.na(expcat)) %>%
    group_by(expcat, edcat5, female, year) %>%
    summarise(lswt = sum(q_lsweight, na.rm = TRUE), .groups = "drop") %>%
    arrange(year, edcat5, expcat, female)
  
  # Save output
  write.csv(result, file.path(OUTPUT_DIR, output_file), row.names = FALSE)
  
  cat(" ✓ (", nrow(result), "groups,", 
      min(result$year, na.rm = TRUE), "-", 
      max(result$year, na.rm = TRUE), ")\n")
  
  return(result)
}

# ==============================================================================
# COMPARISON FUNCTION
# ==============================================================================

compare_with_original <- function(r_data, stata_file_path, tolerance = 1e-6) {
  
  cat("\n=================================================================\n")
  cat("COMPARISON WITH ORIGINAL STATA FILE\n")
  cat("=================================================================\n")
  
  # Check if Stata file exists
  if (!file.exists(stata_file_path)) {
    cat("Original Stata file not found:", stata_file_path, "\n")
    return(FALSE)
  }
  
  # Read Stata file
  cat("Reading original Stata file...\n")
  stata_data <- tryCatch({
    read_dta(stata_file_path)
  }, error = function(e) {
    cat("⚠ Error reading Stata file:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(stata_data)) return(FALSE)
  
  # Convert Stata data to data frame and ensure same structure
  stata_data <- as.data.frame(stata_data)
  
  # Check dimensions
  cat("Dimensions comparison:\n")
  cat("   R data:     ", nrow(r_data), "rows ×", ncol(r_data), "columns\n")
  cat("   Stata data: ", nrow(stata_data), "rows ×", ncol(stata_data), "columns\n")
  
  # Check common variables
  r_vars <- names(r_data)
  stata_vars <- names(stata_data)
  common_vars <- intersect(r_vars, stata_vars)
  
  cat("Common variables (", length(common_vars), "/", length(r_vars), "):\n")
  if (length(common_vars) > 0) {
    cat("   ", paste(common_vars, collapse = ", "), "\n")
  }
  
  missing_in_stata <- setdiff(r_vars, stata_vars)
  if (length(missing_in_stata) > 0) {
    cat("⚠ Variables in R but not in Stata:", paste(missing_in_stata, collapse = ", "), "\n")
  }
  
  missing_in_r <- setdiff(stata_vars, r_vars)
  if (length(missing_in_r) > 0) {
    cat("⚠ Variables in Stata but not in R:", paste(missing_in_r, collapse = ", "), "\n")
  }
  
  # Check year ranges
  cat("\n Year range comparison:\n")
  if ("year" %in% common_vars) {
    r_years <- range(r_data$year, na.rm = TRUE)
    stata_years <- range(stata_data$year, na.rm = TRUE)
    cat("   R data:     ", r_years[1], "-", r_years[2], "\n")
    cat("   Stata data: ", stata_years[1], "-", stata_years[2], "\n")
  }
  
  # Check experience categories
  cat("\n Experience categories:\n")
  if ("expcat" %in% common_vars) {
    r_expcats <- sort(unique(r_data$expcat))
    stata_expcats <- sort(unique(stata_data$expcat))
    cat("   R data:     ", paste(r_expcats, collapse = ", "), "\n")
    cat("   Stata data: ", paste(stata_expcats, collapse = ", "), "\n")
  }
  
  # Check education categories
  cat("\n Education categories:\n")
  if ("edcat5" %in% common_vars) {
    r_edcats <- sort(unique(r_data$edcat5))
    stata_edcats <- sort(unique(stata_data$edcat5))
    cat("   R data:     ", paste(r_edcats, collapse = ", "), "\n")
    cat("   Stata data: ", paste(stata_edcats, collapse = ", "), "\n")
  }
  
  # Merge data for comparison if we have the key variables
  merge_vars <- intersect(c("year", "expcat", "edcat5", "female"), common_vars)
  
  if (length(merge_vars) >= 2) {
    cat("\n Merging datasets for value comparison...\n")
    merged_data <- merge(r_data, stata_data, by = merge_vars, 
                         suffixes = c("_r", "_stata"), all = TRUE)
    
    cat("   Merged observations:", nrow(merged_data), "\n")
    cat("   R-only observations:", sum(is.na(merged_data$lswt_stata)), "\n")
    cat("   Stata-only observations:", sum(is.na(merged_data$lswt_r)), "\n")
    
    # Compare lswt values
    if ("lswt_r" %in% names(merged_data) && "lswt_stata" %in% names(merged_data)) {
      cat("\n Labor supply weight (lswt) comparison:\n")
      
      # Filter to valid observations
      valid_indices <- !is.na(merged_data$lswt_r) & !is.na(merged_data$lswt_stata)
      
      if (sum(valid_indices) > 0) {
        r_vals <- merged_data$lswt_r[valid_indices]
        stata_vals <- merged_data$lswt_stata[valid_indices]
        
        # Calculate differences
        abs_diff <- abs(r_vals - stata_vals)
        rel_diff <- abs_diff / pmax(abs(stata_vals), 1e-10)  # Avoid division by zero
        
        max_abs_diff <- max(abs_diff, na.rm = TRUE)
        mean_abs_diff <- mean(abs_diff, na.rm = TRUE)
        max_rel_diff <- max(rel_diff, na.rm = TRUE)
        mean_rel_diff <- mean(rel_diff, na.rm = TRUE)
        
        # Basic statistics
        cat("   R data:        Mean =", sprintf("%.2f", mean(r_vals, na.rm = TRUE)), 
            ", SD =", sprintf("%.2f", sd(r_vals, na.rm = TRUE)), "\n")
        cat("   Stata data:    Mean =", sprintf("%.2f", mean(stata_vals, na.rm = TRUE)), 
            ", SD =", sprintf("%.2f", sd(stata_vals, na.rm = TRUE)), "\n")
        
        # Difference statistics
        cat("   Max abs diff: ", sprintf("%.2e", max_abs_diff), "\n")
        cat("   Mean abs diff:", sprintf("%.2e", mean_abs_diff), "\n")
        cat("   Max rel diff: ", sprintf("%.2f%%", max_rel_diff * 100), "\n")
        cat("   Mean rel diff:", sprintf("%.2f%%", mean_rel_diff * 100), "\n")
        
        # Check if values match within tolerance
        matches <- max_abs_diff < tolerance
        status <- ifelse(matches, "✅", "⚠")
        cat("   ", status, "Values match within tolerance (", sprintf("%.2e", tolerance), ")\n")
        
        # Correlation
        corr <- cor(r_vals, stata_vals, use = "complete.obs")
        cat("   Correlation:  ", sprintf("%.6f", corr), "\n")
        
        # Show some sample differences if there are significant ones
        if (max_abs_diff >= tolerance && sum(valid_indices) >= 5) {
          # Find indices with largest differences
          large_diff_indices <- which(valid_indices)[order(abs_diff, decreasing = TRUE)[1:min(5, sum(valid_indices))]]
          
          cat("\n   Sample observations with largest differences:\n")
          for (i in large_diff_indices) {
            diff_val <- merged_data$lswt_r[i] - merged_data$lswt_stata[i]
            cat("     ", sprintf("Year %d, ExpCat %d, EdCat %d, Female %d: R = %.2f, Stata = %.2f, Diff = %.2e\n",
                                 merged_data$year[i], merged_data$expcat[i], merged_data$edcat5[i], 
                                 merged_data$female[i], merged_data$lswt_r[i], merged_data$lswt_stata[i], diff_val))
          }
        }
        
        # Summary by categories
        cat("\n Summary statistics by categories:\n")
        
        # By year (show a few sample years)
        if ("year" %in% merge_vars && length(unique(merged_data$year)) > 5) {
          sample_years <- sort(unique(merged_data$year))[seq(1, length(unique(merged_data$year)), 
                                                             length.out = min(5, length(unique(merged_data$year))))]
          for (yr in sample_years) {
            yr_data <- merged_data[merged_data$year == yr & valid_indices, ]
            if (nrow(yr_data) > 0) {
              yr_corr <- cor(yr_data$lswt_r, yr_data$lswt_stata, use = "complete.obs")
              yr_mean_diff <- mean(abs(yr_data$lswt_r - yr_data$lswt_stata), na.rm = TRUE)
              cat("     Year", yr, ": Corr =", sprintf("%.4f", yr_corr), 
                  ", Mean abs diff =", sprintf("%.2e", yr_mean_diff), "\n")
            }
          }
        }
        
        return(matches)
        
      } else {
        cat("   ⚠ No valid observations to compare\n")
        return(FALSE)
      }
    } else {
      cat("   ⚠ Cannot find lswt variable in both datasets\n")
      return(FALSE)
    }
  } else {
    cat("   ⚠ Cannot merge data - insufficient common key variables\n")
    cat("   Available merge variables:", paste(merge_vars, collapse = ", "), "\n")
    return(FALSE)
  }
}

# ==============================================================================
# EXECUTION
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("=================================================================\n")
cat("March Labor Supply Weights Assembly Script - Enhanced with Comparison\n")
cat("=================================================================\n")

start_time <- Sys.time()

# Process each file pair
results <- list()
for (file_pair in FILES) {
  results[[file_pair$output]] <- assemble_lswts_exp(file_pair$input, file_pair$output)
  
  # Run comparison if specified
  if (!is.null(file_pair$compare) && file_pair$compare) {
    comparison_result <- compare_with_original(results[[file_pair$output]], COMPARISON_FILE)

  }
}

cat("\n Created", length(results), "output files in", 
    round(difftime(Sys.time(), start_time, units = "secs"), 2), "seconds\n")
cat("=================================================================\n")