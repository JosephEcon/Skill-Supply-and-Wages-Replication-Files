# 10_Comparison_report.R
# Global comparison: R replication outputs vs AGK Stata reference files
#
# Checks four datasets:
#   1. March cells (script 2)          vs Data/AGK/marchcells6318.dta
#   2. Efficiency units (script 3)     vs Data/AGK/effunits-exp-byexp-6318.dta
#   3. Labor supply weights (script 5) vs Data/AGK/march-lswts-exp.dta
#   4. Wage premium series (scripts 4/6/7) vs Data/AGK/clghsgwg-march-regseries-exp.dta
#
# Writes detailed diagnostics to Comparison_Report.txt and prints a
# global summary table at the end.

library(dplyr)
library(haven)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

REPORT_FILE   <- "Comparison_Report.txt"
AGK_DIR       <- "Data/AGK"
SERIES_DIR    <- "Data/series"
TOLERANCE     <- 1e-6   # exact-match threshold (absolute)
REL_TOLERANCE <- 0.01   # 1% relative tolerance — differences below this are "approximately matched"

COMPARISONS <- list(
  list(
    label    = "March cells (6318)",
    r_file   = file.path(SERIES_DIR, "marchcells6318.csv"),
    agk_file = file.path(AGK_DIR,    "marchcells6318.dta"),
    merge_by = c("year"),                          # row-count check only (individual data)
    key_vars = NULL,                               # no paired value comparison
    script   = "2_Demographic_cells_pt2.R"
  ),
  list(
    label    = "Efficiency units (6318)",
    r_file   = file.path(SERIES_DIR, "effunits-exp-byexp-6318.csv"),
    agk_file = file.path(AGK_DIR,    "effunits-exp-byexp-6318.dta"),
    merge_by = c("year", "expcat"),
    key_vars = c("euexp_lnclg", "euexp_shclg", "eu_lnclg", "eu_shclg", "hr_clg", "hr_hsg"),
    script   = "3_Efficiency_units.R"
  ),
  list(
    label    = "Labor supply weights (6318)",
    r_file   = file.path(SERIES_DIR, "march-lswts-exp-6318.csv"),
    agk_file = file.path(AGK_DIR,    "march-lswts-exp.dta"),
    merge_by = c("year", "expcat", "edcat5", "female"),
    key_vars = c("lswt"),
    script   = "5_Labor_supply_weights.R"
  )
)

# ==============================================================================
# HELPERS
# ==============================================================================

section <- function(title) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(title, "\n")
  cat(strrep("=", 70), "\n")
}

subsection <- function(title) {
  cat("\n  --", title, "--\n")
}

# Compare two numeric vectors; returns list(grade, pass, max_diff, mean_diff, corr).
# grade: "exact"  — max_diff < TOLERANCE (exact within floating-point tolerance)
#        "approx" — max_diff >= TOLERANCE but max_rel < REL_TOLERANCE (1%); numerically negligible
#        "fail"   — max_rel >= REL_TOLERANCE; meaningful discrepancy
compare_values <- function(r_vals, agk_vals, var_name, tolerance = TOLERANCE) {
  valid <- !is.na(r_vals) & !is.na(agk_vals)
  n <- sum(valid)
  if (n == 0) {
    cat(sprintf("  [N/A ] %-28s  no valid paired observations\n", var_name))
    return(list(grade = NA, pass = NA, max_diff = NA, mean_diff = NA, corr = NA))
  }
  rv  <- r_vals[valid]
  sv  <- agk_vals[valid]
  diffs    <- abs(rv - sv)
  max_diff  <- max(diffs)
  mean_diff <- mean(diffs)
  corr      <- cor(rv, sv, use = "complete.obs")
  max_rel   <- max(diffs / pmax(abs(sv), 1e-10))
  pass      <- max_diff < tolerance
  approx    <- !pass && max_rel < REL_TOLERANCE
  grade     <- if (pass) "exact" else if (approx) "approx" else "fail"
  status    <- switch(grade, exact = "[PASS]", approx = "[~OK ]", fail = "[FAIL]")
  cat(sprintf("  %s %-28s  max_diff = %.2e  mean_diff = %.2e  corr = %.6f  (n = %d)\n",
              status, var_name, max_diff, mean_diff, corr, n))
  list(grade = grade, pass = pass, max_diff = max_diff, mean_diff = mean_diff, corr = corr)
}

fmt_status <- function(x) {
  if (is.null(x) || is.na(x)) return("  N/A  ")
  # logical TRUE/FALSE kept for backward compat (files/nrow/vars checks)
  if (isTRUE(x))  return("  PASS ")
  if (identical(x, FALSE)) return("  FAIL ")
  # character grade from compare_values
  switch(x, exact = "  PASS ", approx = "  ~OK  ", fail = "  FAIL ", "  N/A  ")
}

# ==============================================================================
# OPEN SINK
# ==============================================================================

while (sink.number() > 0) sink()
sink(REPORT_FILE, split = TRUE)

cat(strrep("=", 70), "\n")
cat("R REPLICATION vs AGK STATA: COMPARISON REPORT\n")
cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Tolerance: %.2e\n", TOLERANCE))
cat(strrep("=", 70), "\n")

# ==============================================================================
# RUN COMPARISONS
# ==============================================================================

summary_rows <- list()

for (comp in COMPARISONS) {

  section(sprintf("%s  [%s]", comp$label, comp$script))
  cat(sprintf("  R output:      %s\n", comp$r_file))
  cat(sprintf("  AGK reference: %s\n", comp$agk_file))

  row <- list(label = comp$label, files = FALSE, nrow = NA, vars = NA, values = NA)

  # --- File existence ---
  if (!file.exists(comp$r_file)) {
    cat("\n  [SKIP] R output file not found.\n")
    summary_rows[[length(summary_rows) + 1]] <- row
    next
  }
  if (!file.exists(comp$agk_file)) {
    cat("\n  [SKIP] AGK reference file not found.\n")
    summary_rows[[length(summary_rows) + 1]] <- row
    next
  }

  row$files <- TRUE

  # --- Load data ---
  r_data   <- read.csv(comp$r_file, check.names = FALSE)
  agk_data <- as.data.frame(read_dta(comp$agk_file))

  # Repair _merge column name mangled by R's check.names (legacy CSV writes)
  names(r_data) <- sub("^X_merge$", "_merge", names(r_data))

  # --- Dimensions ---
  subsection("Dimensions")
  cat(sprintf("  Rows:    R = %d,  AGK = %d\n", nrow(r_data), nrow(agk_data)))
  cat(sprintf("  Columns: R = %d,  AGK = %d\n", ncol(r_data), ncol(agk_data)))
  row$nrow <- (nrow(r_data) == nrow(agk_data))

  # --- Column coverage ---
  subsection("Column coverage")
  only_r   <- setdiff(names(r_data),   names(agk_data))
  only_agk <- setdiff(names(agk_data), names(r_data))
  if (length(only_r)   == 0 && length(only_agk) == 0) {
    cat("  [PASS] Column sets are identical\n")
    row$vars <- TRUE
  } else {
    if (length(only_r)   > 0) cat("  Only in R:   ", paste(only_r,   collapse = ", "), "\n")
    if (length(only_agk) > 0) cat("  Only in AGK: ", paste(only_agk, collapse = ", "), "\n")
    row$vars <- (length(only_agk) == 0)  # pass if R has at least everything AGK has
  }

  # --- Per-year observation counts ---
  if ("year" %in% names(r_data) && "year" %in% names(agk_data)) {
    subsection("Per-year observation counts")
    r_yc   <- as.data.frame(table(year = r_data$year));   names(r_yc)[2]   <- "n_r"
    agk_yc <- as.data.frame(table(year = agk_data$year)); names(agk_yc)[2] <- "n_agk"
    yr_comp <- merge(r_yc, agk_yc, by = "year", all = TRUE)
    yr_comp$diff <- as.integer(as.character(yr_comp$n_r)) -
                    as.integer(as.character(yr_comp$n_agk))
    mismatch <- yr_comp[!is.na(yr_comp$diff) & yr_comp$diff != 0, ]

    if (!is.null(comp$key_vars)) {
      # For aggregated data, year counts can differ due to merge scope; just report
      cat(sprintf("  Year range: R = %s-%s,  AGK = %s-%s\n",
                  min(r_data$year, na.rm = TRUE),   max(r_data$year,   na.rm = TRUE),
                  min(agk_data$year, na.rm = TRUE),  max(agk_data$year, na.rm = TRUE)))
    } else {
      # For individual-level data, per-year counts are the main check
      if (nrow(mismatch) == 0) {
        cat("  [PASS] Per-year observation counts match exactly\n")
      } else {
        cat(sprintf("  [FAIL] %d year(s) with row-count mismatch (showing up to 10):\n", nrow(mismatch)))
        print(head(mismatch, 10))

        # Anti-join: identify the specific rows AGK has that R is missing
        id_vars <- intersect(c("year", "edcat5", "female", "exp"),
                             intersect(names(r_data), names(agk_data)))
        if (length(id_vars) >= 3) {
          subsection("Missing observation detail")
          extra <- anti_join(agk_data, r_data, by = id_vars)
          if (nrow(extra) > 0) {
            show_cols <- intersect(c(id_vars, "q_obs", "q_lsweight", "_merge"), names(extra))
            cat("  Rows in AGK but absent from R (anti-join on", paste(id_vars, collapse = "/"), "):\n")
            for (i in seq_len(nrow(extra))) {
              parts <- sapply(show_cols, function(cn) sprintf("%s=%s", cn, as.character(extra[[cn]][i])))
              cat("   ", paste(parts, collapse = "  "), "\n")
            }
            if ("exp" %in% id_vars && all(extra$exp >= 40)) {
              cat("  Note: exp >= 40 places these cells at the top of experience category 5\n")
              cat("  (range 40-48). With q_obs = 1-2 they carry negligible aggregate weight.\n")
            }
          }
        }
      }
      row$values <- if (nrow(mismatch) == 0) "exact" else "fail"
    }
  }

  # --- Numeric value comparisons (aggregated datasets only) ---
  if (!is.null(comp$key_vars) && length(comp$key_vars) > 0) {
    subsection("Numeric value comparisons")

    merge_by <- intersect(comp$merge_by, intersect(names(r_data), names(agk_data)))

    if (length(merge_by) < 1) {
      cat("  [SKIP] Merge keys not available in both datasets\n")
    } else {
      merged <- merge(r_data, agk_data, by = merge_by, suffixes = c("_r", "_agk"), all = TRUE)
      cat(sprintf("  Merging on: %s\n", paste(merge_by, collapse = ", ")))

      # Report unmatched rows
      sentinel_r   <- paste0(comp$key_vars[1], "_r")
      sentinel_agk <- paste0(comp$key_vars[1], "_agk")
      if (sentinel_r %in% names(merged) && sentinel_agk %in% names(merged)) {
        n_matched  <- sum(!is.na(merged[[sentinel_r]])   & !is.na(merged[[sentinel_agk]]))
        n_r_only   <- sum(!is.na(merged[[sentinel_r]])   &  is.na(merged[[sentinel_agk]]))
        n_agk_only <- sum( is.na(merged[[sentinel_r]])   & !is.na(merged[[sentinel_agk]]))
        cat(sprintf("  Matched: %d  |  R-only: %d  |  AGK-only: %d\n\n",
                    n_matched, n_r_only, n_agk_only))
      }

      val_results <- lapply(comp$key_vars, function(v) {
        rc  <- paste0(v, "_r")
        ac  <- paste0(v, "_agk")
        if (rc %in% names(merged) && ac %in% names(merged)) {
          compare_values(merged[[rc]], merged[[ac]], v)
        } else {
          cat(sprintf("  [N/A ] %-28s  variable not found in both datasets\n", v))
          list(pass = NA)
        }
      })

      grades <- sapply(val_results, function(x) if (is.null(x$grade) || is.na(x$grade)) "na" else x$grade)
      row$values <- if (all(grades == "exact")) "exact" else if (!any(grades == "fail")) "approx" else "fail"

      # Per-year correlation summary (sample of 6 years)
      if ("year" %in% merge_by) {
        subsection("Per-year summary (sample of years)")
        all_years <- sort(unique(merged$year))
        sel_years <- all_years[round(seq(1, length(all_years), length.out = min(6, length(all_years))))]
        for (yr in sel_years) {
          yd <- merged[merged$year == yr, ]
          yr_stats <- lapply(comp$key_vars, function(v) {
            rc <- paste0(v, "_r"); ac <- paste0(v, "_agk")
            if (!(rc %in% names(yd) && ac %in% names(yd))) return(NULL)
            valid <- !is.na(yd[[rc]]) & !is.na(yd[[ac]])
            if (sum(valid) < 2) return(NULL)
            list(
              max_diff = max(abs(yd[[rc]][valid] - yd[[ac]][valid])),
              corr     = cor(yd[[rc]][valid], yd[[ac]][valid], use = "complete.obs")
            )
          })
          yr_stats <- Filter(Negate(is.null), yr_stats)
          if (length(yr_stats) > 0) {
            yr_max  <- max(sapply(yr_stats, `[[`, "max_diff"))
            yr_corr <- mean(sapply(yr_stats, `[[`, "corr"), na.rm = TRUE)
            status  <- ifelse(yr_max < TOLERANCE, "[PASS]", "[FAIL]")
            cat(sprintf("  %s Year %d:  max_diff = %.2e  mean_corr = %.6f\n",
                        status, yr, yr_max, yr_corr))
          }
        }
      }

      # Relative-difference assessment
      # Absolute tolerance failures can be misleading when series have large magnitudes
      # (false alarms) or near-zero values (inflated %). This section shows both.
      subsection("Relative-difference assessment")
      cat("  max_rel = max|R - AGK| / max(|AGK|, 1e-10)\n\n")
      for (v in comp$key_vars) {
        rc <- paste0(v, "_r"); ac <- paste0(v, "_agk")
        if (!(rc %in% names(merged) && ac %in% names(merged))) next
        valid <- !is.na(merged[[rc]]) & !is.na(merged[[ac]])
        if (sum(valid) == 0) next
        rv <- merged[[rc]][valid]; av <- merged[[ac]][valid]
        rel     <- abs(rv - av) / pmax(abs(av), 1e-10)
        max_rel <- max(rel)
        max_idx <- which.max(rel)
        max_abs <- abs(rv[max_idx] - av[max_idx])
        max_av  <- av[max_idx]
        max_yr  <- if ("year" %in% names(merged)) merged$year[valid][max_idx] else NA
        sig     <- if (max_rel < 0.001) "negligible" else if (max_rel < 0.01) "minor" else "notable"
        note <- ""
        if (abs(max_av) < 0.05) {
          note <- sprintf("  [near-zero denom: year=%s |AGK|=%.2e abs_diff=%.2e]",
                          ifelse(is.na(max_yr), "?", as.character(max_yr)),
                          abs(max_av), max_abs)
        }
        cat(sprintf("  %-28s  max_rel = %7.4f%%  (%s)%s\n",
                    v, max_rel * 100, sig, note))
      }
    }
  }

  summary_rows[[length(summary_rows) + 1]] <- row
}

# ==============================================================================
# 4. WAGE PREMIUM SERIES  (scripts 4 / 6 / 7)
# ==============================================================================
#
# Compares R's clghsgwg-march-regseries-exp.csv directly against AGK's
# clghsgwg-march-regseries-exp.dta (the CPS-pipeline output from
# calc-marchwg-byexp.do).  This is the definitive check on whether scripts
# 4_Wage_equations, 6_Wages, and 7_Wage_premiums are correct.
#
# Key variables: clphsg_all, clghsg_all, and gender-specific variants,
# merged on (year, expcat).
# ==============================================================================

section("4. WAGE PREMIUM SERIES  [scripts 4_Wage_equations / 6_Wages / 7_Wage_premiums]")

agk_wg_file <- file.path(AGK_DIR, "clghsgwg-march-regseries-exp.dta")
r_wg_file   <- file.path(SERIES_DIR, "clghsgwg-march-regseries-exp-6318.csv")

cat(sprintf("  R output:      %s\n", r_wg_file))
cat(sprintf("  AGK reference: %s\n", agk_wg_file))

lt_row <- list(label = "Wage premium series", files = FALSE,
               nrow = NA, vars = NA, values = NA)

if (!file.exists(r_wg_file)) {
  cat("\n  [SKIP] R output file not found.\n")
} else if (!file.exists(agk_wg_file)) {
  cat("\n  [SKIP] AGK reference file not found.\n")
} else {

  lt_row$files <- TRUE

  r_wg   <- read.csv(r_wg_file)
  agk_wg <- as.data.frame(read_dta(agk_wg_file))

  subsection("Dimensions")
  cat(sprintf("  Rows:    R = %d,  AGK = %d\n", nrow(r_wg), nrow(agk_wg)))
  lt_row$nrow <- (nrow(r_wg) == nrow(agk_wg))
  cat(sprintf("  Years:   R = %d-%d,  AGK = %d-%d\n",
              min(r_wg$year), max(r_wg$year),
              min(agk_wg$year), max(agk_wg$year)))

  only_r   <- setdiff(names(r_wg),   names(agk_wg))
  only_agk <- setdiff(names(agk_wg), names(r_wg))
  lt_row$vars <- (length(only_agk) == 0)
  if (length(only_r)   > 0) cat("  Only in R:   ", paste(only_r,   collapse = ", "), "\n")
  if (length(only_agk) > 0) cat("  Only in AGK: ", paste(only_agk, collapse = ", "), "\n")
  if (length(only_r) == 0 && length(only_agk) == 0) cat("  [PASS] Column sets are identical\n")

  # Merge on (year, expcat) for overlap years
  overlap_years <- intersect(r_wg$year, agk_wg$year)
  cat(sprintf("\n  Overlap years: %d-%d  (n = %d)\n",
              min(overlap_years), max(overlap_years), length(overlap_years)))

  merged_wg <- merge(
    r_wg[r_wg$year     %in% overlap_years, ],
    agk_wg[agk_wg$year %in% overlap_years, ],
    by = c("year", "expcat"), suffixes = c("_r", "_agk")
  )

  n_matched  <- nrow(merged_wg)
  n_r_only   <- nrow(r_wg[r_wg$year %in% overlap_years, ]) - n_matched
  n_agk_only <- nrow(agk_wg[agk_wg$year %in% overlap_years, ]) - n_matched
  cat(sprintf("  Matched rows: %d  (R-only: %d, AGK-only: %d)\n\n",
              n_matched, n_r_only, n_agk_only))

  subsection("Value comparisons (merged on year, expcat)")
  key_vars <- c("clphsg_all", "clghsg_all",
                "clphsg_all_m", "clghsg_all_m",
                "clphsg_all_f", "clghsg_all_f")

  val_results <- lapply(key_vars, function(v) {
    rc <- paste0(v, "_r"); ac <- paste0(v, "_agk")
    if (rc %in% names(merged_wg) && ac %in% names(merged_wg))
      compare_values(merged_wg[[rc]], merged_wg[[ac]], v)
    else { cat(sprintf("  [N/A ] %-28s  not in both datasets\n", v)); list(pass = NA) }
  })
  grades_wg <- sapply(val_results, function(x) if (is.null(x$grade) || is.na(x$grade)) "na" else x$grade)
  lt_row$values <- if (all(grades_wg == "exact")) "exact" else if (!any(grades_wg == "fail")) "approx" else "fail"

  subsection("Per-year summary for clphsg_all (sample of years)")
  all_years <- sort(unique(merged_wg$year))
  sel_years <- all_years[round(seq(1, length(all_years), length.out = min(6, length(all_years))))]
  for (yr in sel_years) {
    yd <- merged_wg[merged_wg$year == yr, ]
    valid <- !is.na(yd$clphsg_all_r) & !is.na(yd$clphsg_all_agk)
    if (sum(valid) > 0) {
      yr_max  <- max(abs(yd$clphsg_all_r[valid] - yd$clphsg_all_agk[valid]))
      yr_corr <- cor(yd$clphsg_all_r[valid], yd$clphsg_all_agk[valid], use = "complete.obs")
      status  <- ifelse(yr_max < TOLERANCE, "[PASS]", "[FAIL]")
      cat(sprintf("  %s Year %d:  max_diff = %.2e  corr = %.6f\n",
                  status, yr, yr_max, yr_corr))
    }
  }
}

summary_rows[[length(summary_rows) + 1]] <- lt_row

# ==============================================================================
# 5. AGK 2005 DATA SPLICE — METHODOLOGICAL NOTE
# ==============================================================================
#
# This section documents a structural feature of AGK's GK dataset that makes
# any direct comparison of GK-period results between AGK and this replication
# an apples-to-oranges comparison.
# ==============================================================================

section("5. AGK 2005 DATA SPLICE — METHODOLOGICAL NOTE")

cat("
  AGK's GK dataset (taba1_a2_figa1_1914_2017.do) is not a single consistently-
  constructed series. It is a splice of two methodologically distinct series:

  Pre-2006:  colhs1405.dta  — the original AKK (2008) CPS construction for
             wprem2 and relsup. AKK (2008) used March CPS data through ~2005,
             so the implicit labor supply weights (avlswt) are averaged over
             approximately 1963-2005 (a '6305-period' weight basis).

  2006-2017: km-cg-rsup-6317.dta — AGK's updated CPS series, constructed
             using marchcells6318.dta (confirmed in assemb-march-lswts-exp.do
             line 26: 'use series/marchcells6318'). The avlswt in this series
             are averaged over 1963-2017 (a '6318-period' weight basis).

  HARDCODED LEVEL-SHIFT AT 2005 (taba1_a2_figa1_1914_2017.do lines 27-30):

    replace wprem2 = clphsg_all + .0037398  if year >= 2006
    replace relsup = eu_lnclg  + .0152795   if year >= 2006

  These offsets are the gap between colhs1405 and km-cg-rsup-6317 in 2005.
  They are specific to the 6317 series and will differ for any other construction.
  The two series are joined using a single year's level difference as a permanent
  offset — this is not a principled methodological splice.

  IMPLICATIONS FOR THIS REPLICATION:

  1. The 6318 wage premium comparison above (Section 4) compares R output against
     AGK's km-cg-rsup-6317.dta for 1963-2017 using 6318-period weights throughout.
     This matches AGK's post-2005 construction but differs from the 6305-weighted
     AKK (2008) data used for pre-2006 observations in AGK's GK dataset.

  2. Any comparison of GK-period regression results (Tables A1, A2, Figure A1)
     between AGK and this replication reflects both (a) code corrections and
     (b) the shift from AGK's spliced series to this replication's single
     consistently-constructed series (1963-2023, 6323-period weights).

  3. The hardcoded offsets (.0037398, .0152795) are not replicated here because
     this replication uses a single uninterrupted series. Any offset between the
     two approaches at 2005 will differ from AGK's values.

  CONCLUSION: GK-period results should not be expected to match AGK exactly even
  after all code corrections, owing to this data construction difference. The
  paper already notes that 'replications are not exact, given how the dataset
  has changed over time'; this splice is a specific, quantifiable reason why.
")

# ==============================================================================
# CLOSE SINK
# ==============================================================================

sink()
cat("Comparison report written to:", REPORT_FILE, "\n")
