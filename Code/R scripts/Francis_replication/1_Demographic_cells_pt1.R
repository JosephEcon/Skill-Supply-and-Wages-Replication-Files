# CPS March Data Processing Script - CORRECTED VERSION
# R replication of prepmarchcell.do

library(readr)
library(dplyr)
library(tidyr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SINGLE_YEAR <- NULL  # Set to specific year or NULL for range
START_YEAR <- 1964
END_YEAR <- 2024
DATA_DIR <- "Data/CPS/cleaned"

# ==============================================================================
# DEFLATOR REBASING FUNCTION
# ==============================================================================

rebase_deflator_to_2008 <- function() {
  cat("Rebasing deflator to 2008 = 1.0...\n")
  
  # Read 2009 data (mar09.csv) which contains 2008 earnings to get base deflator value
  y_2009 <- 2009 - 1900
  if (y_2009 >= 100 & y_2009 < 110) {
    file_2009 <- file.path(DATA_DIR, paste0("mar0", y_2009 - 100, ".csv"))
  } else {
    file_2009 <- file.path(DATA_DIR, paste0("mar", y_2009 - 100, ".csv"))
  }
  
  if (!file.exists(file_2009)) {
    stop("Cannot find 2009 data (which contains 2008 earnings) to determine base deflator value")
  }
  
  data_2009 <- read_csv(file_2009, show_col_types = FALSE)
  gdp_2008_base <- as.numeric(data_2009$gdp[1])  # This should be the 2008 deflator
  
  cat("2008 base deflator value (from mar09.csv):", gdp_2008_base, "\n")
  return(gdp_2008_base)
}

# Get the 2008 base deflator value once
GDP_2008_BASE <- rebase_deflator_to_2008()

# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================

prep_march_cell <- function(year) {
  
  if (!(year >= 1964 & year <= 2024)) stop("Year must be between 1964 and 2024")
  
  # Determine file path
  y <- year - 1900
  if (y > 60 & y < 100) {
    file_path <- file.path(DATA_DIR, paste0("mar", y, ".csv"))
  } else if (y >= 100 & y < 110) {
    file_path <- file.path(DATA_DIR, paste0("mar0", y - 100, ".csv"))
  } else {
    file_path <- file.path(DATA_DIR, paste0("mar", y - 100, ".csv"))
  }
  
  if (!file.exists(file_path)) stop(paste("File not found:", file_path))
  
  # Read and process data
  data <- read_csv(file_path, show_col_types = FALSE) %>%
    filter(agely >= 16 & agely <= 64)
  
  # Convert labelled variables to numeric
  labelled_vars <- c("agely", "fulltime", "fullyear", "bcwkwgkm", "bchrwgkm", "allocated",
                     "selfemp", "wgt", "_wkslyr", "hrslyr", "exp", "female", "year",
                     "gdp", "winc_ws", "hinc_ws")
  
  for (var in labelled_vars) {
    if (var %in% names(data)) data[[var]] <- as.numeric(data[[var]])
  }
  
  # Rebase GDP deflator to 2008 = 1.0
  if (!is.na(GDP_2008_BASE) && GDP_2008_BASE > 0) {
    data <- data %>%
      mutate(gdp = gdp / GDP_2008_BASE)
  }
  
  # Filter data - drop observations with missing education data (matches Stata)
  if (year <= 1991) {
    data <- data %>% filter(!is.na(grdhi))
  }
  
  # Education categories
  if (year <= 1991) {
    data <- data %>%
      mutate(
        grdhi = as.numeric(grdhi),
        grdcom = as.numeric(grdcom),
        grdcom = ifelse(grdcom == 3, 1, grdcom),
        educomp = pmax(0, grdhi - (grdcom == 2)),
        ed8 = educomp <= 8,
        ed9 = educomp == 9,
        ed10 = educomp == 10,
        ed11 = educomp == 11,
        edhsg = educomp == 12 & grdcom == 1,
        edsmc = (educomp >= 13 & educomp <= 15) | (educomp == 12 & grdcom == 2),
        edclg = educomp == 16 | educomp == 17,
        edgtc = educomp > 17
      )
  } else {
    data <- data %>%
      mutate(
        grdatn = as.numeric(grdatn),
        ed8 = grdatn <= 34,
        ed9 = grdatn == 35,
        ed10 = grdatn == 36,
        ed11 = grdatn == 37,
        edhsg = grdatn == 38 | grdatn == 39,
        edsmc = grdatn >= 40 & grdatn <= 42,
        edclg = grdatn == 43,
        edgtc = grdatn >= 44
      )
  }
  
  # Create derived variables
  data <- data %>%
    mutate(
      edhsd = ed8 + ed9 + ed10 + ed11,
      edcat = ed8 + 2*ed9 + 3*ed10 + 4*ed11 + 5*edhsg + 6*edsmc + 7*edclg + 8*edgtc,
      edcat8 = edcat,
      edcat5 = pmax(edcat8 - 3, 1),
      hsd = (edcat5 == 1), hsg = (edcat5 == 2), smc = (edcat5 == 3),
      clg = (edcat5 == 4), gtc = (edcat5 == 5),
      exp = round(exp, 0),  # Use integers to match reference cell structure
      ftfy = fulltime * fullyear,
      
      # Earnings (replace as missing if not FTFY or top/bottom coded).
      # Setting hinc_ws to NA for non-FTFY workers restricts hourly wage cell
      # means to full-time full-year workers only. This matches the AGK Stata
      # original (prepmarchcell.do): the "all workers" label in downstream
      # comments refers to all FTFY workers, not all CPS respondents.
      winc_ws = ifelse(!ftfy | bcwkwgkm, NA, winc_ws),
      hinc_ws = ifelse(!ftfy | bchrwgkm, NA, hinc_ws),
      # Real wage conversion: gdp has been rebased to 2008 = 1.0 above, so
      # log(gdp) is negative before 2008 and positive after. Adding log(gdp)
      # to log nominal wages converts to real 2008 dollars. This matches the
      # AGK Stata original: "gen rplnhrw = plnhrw + ln(gdp)" in
      # assemb-marchwg-regs-exp.do.  The equivalent level operation is
      # rwinc = winc_ws * gdp (multiplying by a fraction < 1 for pre-2008
      # years deflates; > 1 for post-2008 inflates to the 2008 base).
      rwinc = winc_ws * gdp,
      lnwinc = log(winc_ws),
      rlnwinc = lnwinc + log(gdp),
      rhinc = hinc_ws * gdp,
      lnhinc = log(hinc_ws),
      rlnhinc = lnhinc + log(gdp),
      
      # Count variables
      q_obs = 1,
      q_weight = wgt,
      q_lsweight = wgt * wkslyr,
      q_lshrsweight = wgt * wkslyr * hrslyr,
      
      # Experience interactions
      exphsd = exp * hsd, exphsg = exp * hsg, expsmc = exp * smc,
      expclg = exp * clg, expgtc = exp * gtc, expsq = exp^2,
      expsqhsd = expsq * hsd, expsqhsg = expsq * hsg, expsqsmc = expsq * smc,
      expsqclg = expsq * clg, expsqgtc = expsq * gtc,
      
      # Price variables (excluding allocators and self-employed)
      p_obs = ifelse(is.na(winc_ws) | allocated == 1 | selfemp == 1, 0, 1),
      p_weight = ifelse(is.na(winc_ws) | allocated == 1 | selfemp == 1, 0, wgt),
      p_lsweight = ifelse(is.na(winc_ws) | allocated == 1 | selfemp == 1, 0, wgt * wkslyr)
    )
  
  # Save pre-collapse data
  write_csv(data, paste0("Data/tmp/precollapsemarch", year, ".csv"))
  
  # ============================================================================
  # CORRECTED MERGE LOGIC TO MATCH STATA
  # ============================================================================
  
  # Step 1: Collapse to cells - quantities (ALL observations)
  collapsed_q <- data %>%
    group_by(year, edcat5, exp, female) %>%
    summarise(
      q_obs = sum(q_obs, na.rm = TRUE),
      q_weight = sum(q_weight, na.rm = TRUE),
      q_lsweight = sum(q_lsweight, na.rm = TRUE),
      q_lshrsweight = sum(q_lshrsweight, na.rm = TRUE),
      p_obs = sum(p_obs, na.rm = TRUE),
      p_weight = sum(p_weight, na.rm = TRUE),
      p_lsweight = sum(p_lsweight, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(year, edcat5, exp, female)
  
  # Step 2: Collapse to cells - means (ONLY where p_weight > 0)
  collapsed_means <- data %>%
    filter(p_weight > 0) %>%
    group_by(year, edcat5, exp, female) %>%
    summarise(
      rwinc = weighted.mean(rwinc, p_weight, na.rm = TRUE),
      rlnwinc = weighted.mean(rlnwinc, p_weight, na.rm = TRUE),
      rhinc = weighted.mean(rhinc, p_weight, na.rm = TRUE),
      rlnhinc = weighted.mean(rlnhinc, p_weight, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(year, edcat5, exp, female)
  
  # Step 3: Merge like Stata (RIGHT JOIN means onto quantities + create _merge)
  # In Stata: merge using quantities onto means
  # means = master dataset, quantities = using dataset
  # We want: right_join(means, quantities) to replicate this
  final_data <- collapsed_q %>%
    left_join(collapsed_means, by = c("year", "edcat5", "exp", "female")) %>%
    mutate(
      # Create _merge variable (use valid R column name)
      merge_var = ifelse(is.na(rwinc), 2, 3)
    ) %>%
    arrange(year, edcat5, exp, female)
  
  # Rename to _merge for output (to match Stata exactly)
  names(final_data)[names(final_data) == "merge_var"] <- "_merge"
  
  # Save final dataset with _merge variable
  write_csv(final_data, paste0("Data/series/marchcells-", year, ".csv"))
  
  cat(year, " ✓\n")
  return(final_data)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Create directories
for (dir in c("Data/tmp", "Data/log", "Data/series")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

if (!dir.exists(DATA_DIR)) {
  stop(paste("Data directory not found:", DATA_DIR))
}

cat("=================================================================\n")
cat("CPS March Data Processing Script\n")
cat("=================================================================\n")

start_time <- Sys.time()

if (!is.null(SINGLE_YEAR)) {
  cat("Processing year:", SINGLE_YEAR, "\n")
  prep_march_cell(SINGLE_YEAR)
  cat("✅ Complete! Output: Data/series/marchcells-", SINGLE_YEAR, ".csv\n")
} else {
  years <- START_YEAR:END_YEAR
  cat("Processing", length(years), "years (", START_YEAR, "-", END_YEAR, ")\n")
  
  success_count <- 0
  for (year in years) {
    result <- tryCatch({
      prep_march_cell(year)
      success_count <- success_count + 1
      TRUE
    }, error = function(e) {
      cat(" ❌ Error:", e$message, "\n")
      FALSE
    })
  }
  
  cat("✅ Successfully processed", success_count, "of", length(years), "years\n")
}

cat("Time:", round(difftime(Sys.time(), start_time, units = "mins"), 2), "minutes\n")
cat("=================================================================\n")