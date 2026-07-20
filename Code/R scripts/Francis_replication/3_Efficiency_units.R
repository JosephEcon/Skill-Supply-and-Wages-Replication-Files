# CPS Efficiency Units Labor Supply by Experience Groups - Corrected Version
# R replication of effunit-supplies-exp-byexp.do

library(dplyr)
library(haven)  # For reading Stata files

# ==============================================================================
# CONFIGURATION
# ==============================================================================

INPUT_DIR <- "Data/series"
OUTPUT_DIR <- "Data/series"
COMPARISON_DIR <- "Data/AGK"

# File ranges to process
RANGES <- list(
  list(suffix = "6387", input = "marchcells6387.csv", output = "effunits-exp-byexp-6387.csv"),
  list(suffix = "6305", input = "marchcells6305.csv", output = "effunits-exp-byexp-6305.csv"),
  list(suffix = "6323", input = "marchcells6323.csv", output = "effunits-exp-byexp-6323.csv"),
  list(suffix = "6318", input = "marchcells6318.csv", output = "effunits-exp-byexp-6318.csv", 
       compare_file = "effunits-exp-byexp-6318.dta", years = c(1963, 2017)),
  list(suffix = "6305-extended", input = "marchcells6323.csv", output = "effunits-exp-byexp-6305-extended.csv",
       weight_years = c(1963, 2005), analysis_years = c(1963, 2023))
)

# ==============================================================================
# MAIN EFFICIENCY UNITS CALCULATION
# ==============================================================================

calculate_efficiency_units <- function(suffix, input_file, output_file, years = NULL, weight_years = NULL, analysis_years = NULL) {
  
  cat("Processing", suffix, "...")
  
  # Load data
  data <- read.csv(file.path(INPUT_DIR, input_file), stringsAsFactors = FALSE)
  
  # Handle fixed weight calculation
  if (!is.null(weight_years) && !is.null(analysis_years)) {
    cat(" (weights:", weight_years[1], "-", weight_years[2], ", analysis:", analysis_years[1], "-", analysis_years[2], ")...")
    
    # Step 1: Calculate fixed efficiency units from weight period
    weight_data <- data %>%
      filter(year >= weight_years[1] & year <= weight_years[2]) %>%
      filter(!is.na(exp) & exp >= 0 & exp <= 48) %>%
      mutate(
        expcat = case_when(
          exp >= 0 & exp <= 9 ~ 1,
          exp >= 10 & exp <= 19 ~ 2,
          exp >= 20 & exp <= 29 ~ 3,
          exp >= 30 & exp <= 39 ~ 4,
          exp >= 40 & exp <= 48 ~ 5,
          TRUE ~ NA_real_
        )
      ) %>%
      group_by(year) %>%
      mutate(refwage = max(ifelse(female == 0 & edcat5 == 2 & exp == 10, rwinc, NA), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(relwage = ifelse(!is.na(rwinc) & !is.na(refwage) & refwage > 0, rwinc / refwage, NA)) %>%
      group_by(edcat5, female, exp) %>%
      mutate(celleu_fixed = mean(relwage, na.rm = TRUE)) %>%
      ungroup()
    
    # Step 2: Create lookup table
    eu_lookup <- weight_data %>%
      dplyr::select(edcat5, female, exp, celleu_fixed) %>%
      distinct()
    
    # Step 3: Apply to analysis period
    data <- data %>%
      filter(year >= analysis_years[1] & year <= analysis_years[2]) %>%
      filter(!is.na(exp) & exp >= 0 & exp <= 48) %>%
      mutate(
        expcat = case_when(
          exp >= 0 & exp <= 9 ~ 1,
          exp >= 10 & exp <= 19 ~ 2,
          exp >= 20 & exp <= 29 ~ 3,
          exp >= 30 & exp <= 39 ~ 4,
          exp >= 40 & exp <= 48 ~ 5,
          TRUE ~ NA_real_
        )
      ) %>%
      left_join(eu_lookup, by = c("edcat5", "female", "exp")) %>%
      mutate(celleu = celleu_fixed) %>%
      dplyr::select(-celleu_fixed)
    
  } else {
    # Standard calculation
    # Filter by years if specified
    if (!is.null(years)) {
      data <- data %>% filter(year >= years[1] & year <= years[2])
      cat(" (", years[1], "-", years[2], ")...")
    }
    
    # Filter to experience range 0-48 and create experience categories
    data <- data %>%
      filter(!is.na(exp) & exp >= 0 & exp <= 48) %>%
      mutate(
        expcat = case_when(
          exp >= 0 & exp <= 9 ~ 1,
          exp >= 10 & exp <= 19 ~ 2,
          exp >= 20 & exp <= 29 ~ 3,
          exp >= 30 & exp <= 39 ~ 4,
          exp >= 40 & exp <= 48 ~ 5,
          TRUE ~ NA_real_
        )
      )
    
    # Calculate efficiency units (relative wages)
    # Base: HSG (edcat5 == 2) Male with 10 years of potential experience
    data <- data %>%
      group_by(year) %>%
      mutate(refwage = max(ifelse(female == 0 & edcat5 == 2 & exp == 10, rwinc, NA), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(relwage = ifelse(!is.na(rwinc) & !is.na(refwage) & refwage > 0, rwinc / refwage, NA)) %>%
      group_by(edcat5, female, exp) %>%
      mutate(celleu = mean(relwage, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # Overall efficiency unit measures
  data <- data %>%
    group_by(year) %>%
    mutate(
      tot_euwt = sum(celleu * q_lshrsweight, na.rm = TRUE),
      tot_euwt_m = sum(celleu * q_lshrsweight * (1 - female), na.rm = TRUE),
      tot_euwt_f = sum(celleu * q_lshrsweight * female, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      sh_euwt = ifelse(tot_euwt > 0, celleu * q_lshrsweight / tot_euwt, 0),
      sh_euwt_m = ifelse(tot_euwt_m > 0 & female == 0, celleu * q_lshrsweight / tot_euwt_m, 0),
      sh_euwt_f = ifelse(tot_euwt_f > 0 & female == 1, celleu * q_lshrsweight / tot_euwt_f, 0)
    ) %>%
    group_by(year) %>%
    mutate(
      eu_shclg = sum(sh_euwt * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      eu_shclg_m = sum(sh_euwt_m * (female == 0) * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      eu_shclg_f = sum(sh_euwt_f * female * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      eu_lnclg = ifelse(eu_shclg > 0 & eu_shclg < 1, log(eu_shclg / (1 - eu_shclg)), NA),
      eu_lnclg_m = ifelse(eu_shclg_m > 0 & eu_shclg_m < 1, log(eu_shclg_m / (1 - eu_shclg_m)), NA),
      eu_lnclg_f = ifelse(eu_shclg_f > 0 & eu_shclg_f < 1, log(eu_shclg_f / (1 - eu_shclg_f)), NA),

      # Absolute log supply measures
      ln_supp_clg = log(tot_euwt * eu_shclg),
      ln_supp_hsg = log(tot_euwt * (1 - eu_shclg))
    )
  
  # Hours worked measures
  data <- data %>%
    group_by(year) %>%
    mutate(
      hr_clg = sum(q_lshrsweight * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_clg_m = sum(q_lshrsweight * (female == 0) * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_clg_f = sum(q_lshrsweight * female * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg = sum(q_lshrsweight * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg_m = sum(q_lshrsweight * (female == 0) * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg_f = sum(q_lshrsweight * female * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(year, expcat) %>%
    mutate(
      hr_clg_exp = sum(q_lshrsweight * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_clg_exp_m = sum(q_lshrsweight * (female == 0) * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_clg_exp_f = sum(q_lshrsweight * female * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg_exp = sum(q_lshrsweight * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg_exp_m = sum(q_lshrsweight * (female == 0) * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      hr_hsg_exp_f = sum(q_lshrsweight * female * ((edcat5 == 1 | edcat5 == 2) + 0.5 * (edcat5 == 3)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # By experience group efficiency unit measures
  data <- data %>%
    group_by(year, expcat) %>%
    mutate(
      # Recalculate local tot_euwt for experience group if needed
      tot_euwt_exp   = sum(celleu * q_lshrsweight, na.rm = TRUE),
      # Within-expcat gender totals (fix: previously used year-level tot_euwt_m/f as denominator)
      tot_euwt_exp_m = sum(celleu * q_lshrsweight * (1 - female), na.rm = TRUE),
      tot_euwt_exp_f = sum(celleu * q_lshrsweight * female,       na.rm = TRUE),

      sh_euwt   = ifelse(tot_euwt_exp   > 0,            celleu * q_lshrsweight / tot_euwt_exp,   0),
      sh_euwt_m = ifelse(tot_euwt_exp_m > 0 & female == 0, celleu * q_lshrsweight / tot_euwt_exp_m, 0),
      sh_euwt_f = ifelse(tot_euwt_exp_f > 0 & female == 1, celleu * q_lshrsweight / tot_euwt_exp_f, 0)
    ) %>%
    # Sum shares to get relative supply by exp group
    mutate(
      euexp_shclg = sum(sh_euwt * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      euexp_shclg_m = sum(sh_euwt_m * (female == 0) * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE),
      euexp_shclg_f = sum(sh_euwt_f * female * ((edcat5 == 4 | edcat5 == 5) + 0.5 * (edcat5 == 3)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      euexp_lnclg = ifelse(euexp_shclg > 0 & euexp_shclg < 1, log(euexp_shclg / (1 - euexp_shclg)), NA),
      euexp_lnclg_m = ifelse(euexp_shclg_m > 0 & euexp_shclg_m < 1, log(euexp_shclg_m / (1 - euexp_shclg_m)), NA),
      euexp_lnclg_f = ifelse(euexp_shclg_f > 0 & euexp_shclg_f < 1, log(euexp_shclg_f / (1 - euexp_shclg_f)), NA)
    )
  
  # Prepare final dataset
  final_vars <- c("year", "expcat",
                  "euexp_lnclg", "euexp_lnclg_m", "euexp_lnclg_f",
                  "euexp_shclg", "euexp_shclg_m", "euexp_shclg_f",
                  "eu_lnclg", "eu_lnclg_m", "eu_lnclg_f",
                  "eu_shclg", "eu_shclg_m", "eu_shclg_f",
                  "ln_supp_clg", "ln_supp_hsg",
                  "hr_clg", "hr_clg_m", "hr_clg_f",
                  "hr_hsg", "hr_hsg_m", "hr_hsg_f",
                  "hr_clg_exp", "hr_clg_exp_m", "hr_clg_exp_f",
                  "hr_hsg_exp", "hr_hsg_exp_m", "hr_hsg_exp_f")
  
  final_data <- data %>%
    dplyr::select(dplyr::any_of(final_vars)) %>%
    group_by(year, expcat) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(year, expcat)
  
  # Save output
  write.csv(final_data, file.path(OUTPUT_DIR, output_file), row.names = FALSE)
  
  cat(" ✓ (", nrow(final_data), "obs,", 
      min(final_data$year, na.rm = TRUE), "-", 
      max(final_data$year, na.rm = TRUE), ")\n")
  
  return(final_data)
}

# ==============================================================================
# COMPARISON FUNCTION (RESTORED)
# ==============================================================================

compare_with_original <- function(r_data, stata_file_path, tolerance = 1e-6) {
  
  cat("\n=================================================================\n")
  cat("COMPARISON WITH ORIGINAL STATA FILE\n")
  cat("=================================================================\n")
  
  # Check if Stata file exists
  if (!file.exists(stata_file_path)) {
    cat("⚠ Original Stata file not found:", stata_file_path, "\n")
    return(FALSE)
  }
  
  # Read Stata file
  cat("📖 Reading original Stata file...\n")
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
  cat("📊 Dimensions comparison:\n")
  cat("   R data:     ", nrow(r_data), "rows ×", ncol(r_data), "columns\n")
  cat("   Stata data: ", nrow(stata_data), "rows ×", ncol(stata_data), "columns\n")
  
  # Check common variables
  r_vars <- names(r_data)
  stata_vars <- names(stata_data)
  common_vars <- intersect(r_vars, stata_vars)
  
  cat("🔗 Common variables (", length(common_vars), "/", length(r_vars), "):\n")
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
  
  # Merge data for comparison
  if ("year" %in% common_vars && "expcat" %in% common_vars) {
    merged_data <- merge(r_data, stata_data, by = c("year", "expcat"), 
                         suffixes = c("_r", "_stata"), all = TRUE)
    
    cat("\n📈 Value comparison for key variables:\n")
    
    # Compare key variables
    key_vars <- intersect(c("euexp_lnclg", "euexp_shclg", "eu_lnclg", "eu_shclg", 
                            "hr_clg", "hr_hsg"), common_vars)
    
    all_match <- TRUE
    
    for (var in key_vars) {
      r_col <- paste0(var, "_r")
      stata_col <- paste0(var, "_stata")
      
      if (r_col %in% names(merged_data) && stata_col %in% names(merged_data)) {
        
        # Remove missing values for comparison
        valid_indices <- !is.na(merged_data[[r_col]]) & !is.na(merged_data[[stata_col]])
        
        if (sum(valid_indices) > 0) {
          r_vals <- merged_data[[r_col]][valid_indices]
          stata_vals <- merged_data[[stata_col]][valid_indices]
          
          # Calculate differences
          max_diff <- max(abs(r_vals - stata_vals), na.rm = TRUE)
          mean_diff <- mean(abs(r_vals - stata_vals), na.rm = TRUE)
          
          # Check if values match within tolerance
          matches <- max_diff < tolerance
          all_match <- all_match && matches
          
          status <- ifelse(matches, "✅", "⚠")
          cat("   ", status, var, ": max diff =", sprintf("%.2e", max_diff), 
              ", mean diff =", sprintf("%.2e", mean_diff), "\n")
          
          # Show some sample comparisons if there are differences
          if (!matches && sum(valid_indices) >= 5) {
            sample_indices <- sample(which(valid_indices), min(5, sum(valid_indices)))
            cat("      Sample differences:\n")
            for (i in sample_indices) {
              diff_val <- merged_data[[r_col]][i] - merged_data[[stata_col]][i]
              cat("        Year", merged_data$year[i], "ExpCat", merged_data$expcat[i], 
                  ": R =", round(merged_data[[r_col]][i], 6), 
                  ", Stata =", round(merged_data[[stata_col]][i], 6), 
                  ", Diff =", sprintf("%.2e", diff_val), "\n")
            }
          }
        } else {
          cat("   ⚠", var, ": No valid observations to compare\n")
        }
      }
    }
    
    return(all_match)
    
  } else {
    cat("⚠ Cannot merge data - missing year or expcat variables\n")
    return(FALSE)
  }
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create output directory
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("=================================================================\n")
cat("CPS Efficiency Units Labor Supply Script - Enhanced Version\n")
cat("=================================================================\n")

start_time <- Sys.time()

# Process each range
results <- list()
for (range in RANGES) {
  years_param <- if(!is.null(range$years)) range$years else NULL
  weight_years_param <- if(!is.null(range$weight_years)) range$weight_years else NULL
  analysis_years_param <- if(!is.null(range$analysis_years)) range$analysis_years else NULL
  
  results[[range$suffix]] <- calculate_efficiency_units(range$suffix, range$input, range$output, 
                                                        years_param, weight_years_param, analysis_years_param)
  
  # Run comparison if specified
  if (!is.null(range$compare_file)) {
    comparison_file <- file.path(COMPARISON_DIR, range$compare_file)
    compare_with_original(results[[range$suffix]], comparison_file)
  }
}

cat("\n Created", length(results), "efficiency unit files in", 
    round(difftime(Sys.time(), start_time, units = "mins"), 2), "minutes\n")
cat("=================================================================\n")