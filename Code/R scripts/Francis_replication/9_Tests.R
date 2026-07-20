# ======================================================================
# REPLICATION: KM (1992), AKK (2008), GK (2008)
# Produces Tables 1-7 and Figures 1-3
# ======================================================================

# --- SETUP ---
# Close any sink left open by a previous crashed run, then open fresh
while (sink.number() > 0) sink()
output_file <- "Replication_Results.txt"
sink(output_file)

cat("======================================================================\n")
cat("REPLICATION RESULTS\n")
cat(paste("Analysis date:", Sys.time(), "\n"))
cat("======================================================================\n\n")

# makicoint provides the coint_maki() function, which generalizes the Gregory-Hansen
# test to multiple breaks and is available on CRAN. Set m = 1 for the GH test.
essential_packages <- c("dplyr", "haven", "tseries", "relaimpo", "urca", "sandwich", "makicoint", "ARDL", "AER", "vars")
for (pkg in essential_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}
library(dplyr, warn.conflicts = FALSE)

add_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.001) return("***")
  if (pval < 0.01) return("**")
  if (pval < 0.05) return("*")
  if (pval < 0.10) return(".")
  return("")
}

setup_plot <- function() {
  par(
    pin = c(6.8, 4.7),
    mai = c(0.6, 1.2, 0.2, 1.2),
    family = "sans",
    cex = 1.2, cex.axis = 1.2, cex.lab = 1.2,
    tck = 0.01, mgp = c(3.5, 0.8, 0), las = 1
  )
}

print_table <- function(models, vars_code, var_names, col_labels, use_adj_r2 = TRUE) {
  n_models <- length(models)
  cat(sprintf("%-30s", ""))
  for (l in col_labels) cat(sprintf("%-16s", l))
  cat("\n")
  cat(strrep("-", 30 + 16 * n_models), "\n")
  
  for (v in seq_along(vars_code)) {
    cat(sprintf("%-30s", var_names[v]))
    for (m in seq_along(models)) {
      cfs <- summary(models[[m]])$coefficients
      if (vars_code[v] %in% rownames(cfs)) {
        est <- cfs[vars_code[v], 1]
        pval <- cfs[vars_code[v], 4]
        cat(sprintf("%-16s", paste0(sprintf("%.3f", est), add_stars(pval))))
      } else {
        cat(sprintf("%-16s", ""))
      }
    }
    cat("\n")
    cat(sprintf("%-30s", ""))
    for (m in seq_along(models)) {
      cfs <- summary(models[[m]])$coefficients
      if (vars_code[v] %in% rownames(cfs)) {
        se <- cfs[vars_code[v], 2]
        cat(sprintf("%-16s", paste0("(", sprintf("%.3f", se), ")")))
      } else {
        cat(sprintf("%-16s", ""))
      }
    }
    cat("\n")
  }
  
  cat(strrep("-", 30 + 16 * n_models), "\n")
  cat(sprintf("%-30s", "Observations"))
  for (m in models) cat(sprintf("%-16d", nobs(m)))
  cat("\n")
  
  r2_label <- ifelse(use_adj_r2, "Adjusted R-squared", "R-squared")
  cat(sprintf("%-30s", r2_label))
  for (m in models) {
    r2 <- ifelse(use_adj_r2, summary(m)$adj.r.squared, summary(m)$r.squared)
    cat(sprintf("%-16.3f", r2))
  }
  cat("\n")
}

# ======================================================================
# SPLICING HELPER
# ======================================================================
splice_historical <- function(hist_file, modern_csv,
                              hist_years = c(1914, 1939, 1949, 1959),
                              anchor_year = 1969,
                              modern_rename = c(wprem2 = "clphsg_all",
                                                relsup = "eu_lnclg")) {
  hist_full <- haven::read_dta(hist_file)
  modern <- read.csv(modern_csv) %>% rename(!!!modern_rename) %>% dplyr::select(year, wprem2, relsup)
  hist_pts <- hist_full %>% filter(year %in% hist_years) %>% dplyr::select(year, wprem2, relsup)
  
  hist_anchor <- hist_full %>% filter(year == anchor_year)
  modern_anchor <- modern %>% filter(year == anchor_year)
  offset_w <- modern_anchor$wprem2[1] - hist_anchor$wprem2[1]
  offset_s <- modern_anchor$relsup[1] - hist_anchor$relsup[1]
  
  hist_pts <- hist_pts %>% mutate(wprem2 = wprem2 + offset_w, relsup = relsup + offset_s)
  bind_rows(hist_pts, modern) %>% arrange(year)
}

# ======================================================================
# DATA
# ======================================================================
# KM (1992): 1963-1987
# Time trend starts at 0 in 1963 (time = year - 1963) throughout this script.
# 8_Analysis.R uses t = year - 1962 (starting at 1). The two conventions are
# equivalent for OLS: they differ only by a constant shift, which is absorbed
# by the intercept. For polynomial terms (t^2, t^3), OLS coefficients will
# differ numerically but the fitted values are identical because the intercept
# and lower-order terms adjust to compensate.
km_data <- read.csv("Data/km-cg-rsup-6387.csv") %>%
  rename(wprem = clphsg_all, relsup = eu_lnclg) %>%
  mutate(time = year - 1963) %>%
  filter(year >= 1963 & year <= 1987)

# KM OOS: 1963-2005
km_oos <- read.csv("Data/km-cg-rsup-6305.csv") %>%
  rename(wprem = clphsg_all, relsup = eu_lnclg) %>%
  mutate(time = year - 1963)

# AKK (2008): 1963-2005
akk_data <- read.csv("Data/km-plot-6305.csv") %>%
  rename(wprem = clphsg_all, relsup = eu_lnclg) %>%
  filter(year >= 1963 & year <= 2005) %>%
  mutate(time = year - 1963, time2_sc = (time^2)/100, time3_sc = (time^3)/1000,
         post92 = ifelse(year > 1992, 1, 0), time_post92 = (year - 1992)*post92)

# AKK backcast
akk_oos <- splice_historical("Data/colhs1405.dta", "Data/km-plot-6305.csv") %>%
  rename(wprem = wprem2) %>%
  mutate(time = year - 1963, post92 = ifelse(year > 1992, 1, 0), time_post92 = (year - 1992)*post92)

# GK (2008): 1914-2023
gk_master <- splice_historical("Data/colhs1405.dta", "Data/km-plot-6323.csv") %>%
  mutate(time = year - 1914, time_post49 = pmax(year - 1949, 0), time_post59 = pmax(year - 1959, 0),
         time_post92 = pmax(year - 1992, 0), dummy_1949 = as.numeric(year == 1949),
         relsup_post49 = relsup * as.numeric(year > 1949), time2_sc = time^2/10,
         time3_sc = time^3/1000, time4_sc = time^4/100000)

gk_train <- gk_master %>% filter(year <= 2005)

# Continuous modern dataset (1963-2023)
ts_data <- read.csv("Data/km-plot-6323.csv") %>%
  rename(wprem2 = clphsg_all, relsup = eu_lnclg) %>%
  arrange(year) %>%
  filter(year >= 1963 & year <= 2023) %>%
  mutate(post92 = as.numeric(year >= 1993),
         time = year - 1963)

# ======================================================================
# TABLE 1: KATZ & MURPHY (1992), 1963-1987
# ======================================================================
cat("======================================================================\n")
cat("TABLE 1: KATZ & MURPHY (1992), 1963-1987\n")
cat("======================================================================\n\n")
km_model <- lm(wprem ~ relsup + time, data = km_data)
print_table(list(km_model), c("relsup", "time", "(Intercept)"),
            c("Log relative supply", "Time trend", "Constant"), "(1)")
cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

# ======================================================================
# FIGURE 1: KM (1992) OUT-OF-SAMPLE, 1963-2005
# ======================================================================
km_oos_train <- km_oos %>% filter(year >= 1963 & year <= 1987)
km_plot_model <- lm(wprem ~ relsup + time, data = km_oos_train)
km_pred <- predict(km_plot_model, newdata = km_oos)

pdf("KM_1992_Prediction.pdf", width = 9.2, height = 5.5)
setup_plot()
plot(NULL, xlim = c(1958, 2012), ylim = c(0.35, 0.85), type = "n", xlab = "", ylab = "Log college wage premium", xaxs = "i", yaxs = "i", axes = FALSE)
axis(1, at = seq(1960, 2010, by = 10), lwd = 0, lwd.ticks = 0.8, padj = -0.1)
axis(2, at = seq(0.4, 0.8, by = 0.1), labels = sprintf("%.1f", seq(0.4, 0.8, by = 0.1)), lwd = 0, lwd.ticks = 0.8, padj = 0.4)
box(lwd = 0.8)
lines(km_oos$year, km_oos$wprem, col = "black", lwd = 2)
lines(km_oos$year, km_pred, col = "black", lwd = 1)
abline(v = 1987, lty = 2, col = "black", lwd = 0.5)
text(x = (1958 + 1987)/2, y = 0.375, labels = "Training", cex = 1.2)
text(x = (1987 + 2012)/2, y = 0.375, labels = "Prediction", cex = 1.2)
legend("topleft", legend = c("Actual", "Estimated"), lty = 1, col = "black", lwd = c(2, 1), cex = 1.2, bty = "n")
dev.off()

# Out-of-sample R² for KM (1988-2005 prediction period)
km_holdout_idx <- km_oos$year > 1987
km_oos_r2 <- 1 - sum((km_oos$wprem[km_holdout_idx] - km_pred[km_holdout_idx])^2) /
                  sum((km_oos$wprem[km_holdout_idx] - mean(km_oos$wprem[km_holdout_idx]))^2)

# ======================================================================
# TABLE 2: AUTOR, KATZ & KEARNEY (2008), 1963-2005
# ======================================================================
cat("======================================================================\n")
cat("TABLE 2: AUTOR, KATZ & KEARNEY (2008), 1963-2005\n")
cat("======================================================================\n\n")
akk1 <- lm(wprem ~ relsup + time, data = filter(akk_data, year <= 1987))
akk2 <- lm(wprem ~ relsup + time, data = akk_data)
akk3 <- lm(wprem ~ relsup + time + time_post92, data = akk_data)
akk4 <- lm(wprem ~ relsup + time + time2_sc, data = akk_data)
akk5 <- lm(wprem ~ relsup + time + time2_sc + time3_sc, data = akk_data)
print_table(list(akk1, akk2, akk3, akk4, akk5),
            c("relsup", "time", "time2_sc", "time3_sc", "time_post92", "(Intercept)"),
            c("Log relative supply", "Time trend", "Time^2 / 100", "Time^3 / 1000", "Time trend x post-1992", "Constant"),
            paste0("(", 1:5, ")"))
# Note: time2_sc = time^2/100 and time3_sc = time^3/1000 (AKK data, line 135).
# Table labels show the divisor applied to the variable; reported coefficients
# are for the scaled variable (i.e., multiply coeff by the divisor to recover
# the raw polynomial coefficient).
cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

# ======================================================================
# FIGURE 2: AKK (2008) OUT-OF-SAMPLE BACKCAST, 1914-2005
# ======================================================================
akk_pred <- predict(akk3, newdata = akk_oos)
pdf("AKK_2008_Prediction.pdf", width = 9.2, height = 5.5)
setup_plot()
plot(NULL, xlim = c(1910, 2010), ylim = c(-0.4, 1.0), type = "n", xlab = "", ylab = "Log college wage premium", xaxs = "i", yaxs = "i", axes = FALSE)
axis(1, at = seq(1920, 2000, by = 20), lwd = 0, lwd.ticks = 0.8, padj = -0.1)
axis(2, at = seq(-0.4, 1, by = 0.2), labels = sprintf("%.1f", seq(-0.4, 1, by = 0.2)), lwd = 0, lwd.ticks = 0.8, padj = 0.4)
box(lwd = 0.8)
lines(akk_oos$year, akk_oos$wprem, col = "black", lwd = 2)
lines(akk_oos$year, akk_pred, col = "black", lwd = 1)
abline(v = 1963, lty = 2, col = "black", lwd = 0.5)
text(x = (1963 + 2010)/2, y = -0.33, labels = "Training", cex = 1.2)
text(x = (1910 + 1963)/2, y = -0.33, labels = "Prediction", cex = 1.2)
legend("topleft", legend = c("Actual", "Estimated"), lty = 1, col = "black", lwd = c(2, 1), cex = 1.2, bty = "n")
dev.off()

# Out-of-sample R² for AKK backcast (1914-1962 prediction period)
akk_holdout_idx <- akk_oos$year < 1963
akk_oos_r2 <- 1 - sum((akk_oos$wprem[akk_holdout_idx] - akk_pred[akk_holdout_idx])^2, na.rm = TRUE) /
                   sum((akk_oos$wprem[akk_holdout_idx] - mean(akk_oos$wprem[akk_holdout_idx], na.rm = TRUE))^2, na.rm = TRUE)

# ======================================================================
# TABLE 3: GOLDIN & KATZ (2008), 1914-2005
# ======================================================================
cat("======================================================================\n")
cat("TABLE 3: GOLDIN & KATZ (2008), 1914-2005\n")
cat("======================================================================\n\n")
gk1 <- lm(wprem2 ~ relsup + time + time_post49 + time_post92, data = gk_train)
gk2 <- lm(wprem2 ~ relsup + time + time_post59 + time_post92, data = gk_train)
gk3 <- lm(wprem2 ~ relsup + time + time_post59 + time_post92 + dummy_1949, data = gk_train)
gk4 <- lm(wprem2 ~ relsup + time + time2_sc + time3_sc + time4_sc, data = gk_train)
gk5 <- lm(wprem2 ~ relsup + time + time_post59 + time_post92 + dummy_1949 + relsup_post49, data = gk_train)
print_table(list(gk1, gk2, gk3, gk4, gk5),
            c("relsup", "relsup_post49", "time", "time_post49", "time_post59", "time_post92", "dummy_1949", "time2_sc", "time3_sc", "time4_sc", "(Intercept)"),
            c("Log relative supply", "Log rel supply x post-1949", "Time", "Time x post-1949", "Time x post-1959", "Time x post-1992", "1949 dummy", "Time^2 / 10", "Time^3 / 1000", "Time^4 / 100000", "Constant"),
            paste0("(", 1:5, ")"), use_adj_r2 = FALSE)
cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

# ======================================================================
# FIGURE 3: GK (2008) OUT-OF-SAMPLE, 1914-2023
# ======================================================================
gk_pred <- predict(gk3, newdata = gk_master)
pdf("GK_2008_Prediction.pdf", width = 9.2, height = 5.5)
setup_plot()
plot(NULL, xlim = c(1910, 2030), ylim = c(0.15, 0.85), type = "n", xlab = "", ylab = "Log college wage premium", xaxs = "i", yaxs = "i", axes = FALSE)
axis(1, at = seq(1920, 2020, by = 20), lwd = 0, lwd.ticks = 0.8, padj = -0.1)
axis(2, at = seq(0.2, 0.8, by = 0.1), labels = sprintf("%.1f", seq(0.2, 0.8, by = 0.1)), lwd = 0, lwd.ticks = 0.8, padj = 0.4)
box(lwd = 0.8)
lines(gk_master$year, gk_master$wprem2, col = "black", lwd = 2)
lines(gk_master$year, gk_pred, col = "black", lwd = 1)
abline(v = 2005, lty = 2, col = "black", lwd = 0.5)
text(x = (1910 + 2005)/2, y = 0.185, labels = "Training", cex = 1.2)
text(x = (2005 + 2030)/2, y = 0.185, labels = "Prediction", cex = 1.2)
legend("topleft", legend = c("Actual", "Estimated"), lty = 1, col = "black", lwd = c(2, 1), cex = 1.2, bty = "n")
dev.off()

# Out-of-sample R² for GK (2006-2023 prediction period)
gk_holdout_idx <- gk_master$year > 2005
gk_oos_r2 <- 1 - sum((gk_master$wprem2[gk_holdout_idx] - gk_pred[gk_holdout_idx])^2) /
                  sum((gk_master$wprem2[gk_holdout_idx] - mean(gk_master$wprem2[gk_holdout_idx]))^2)

# ======================================================================
# OUT-OF-SAMPLE R-SQUARED
# ======================================================================
cat("======================================================================\n")
cat("OUT-OF-SAMPLE R-SQUARED\n")
cat("======================================================================\n\n")
cat(sprintf("%-45s %8.2f\n", "KM (1992): 1988-2005 forecast period",  km_oos_r2))
cat(sprintf("%-45s %8.2f\n", "AKK (2008): 1914-1962 backcast period", akk_oos_r2))
cat(sprintf("%-45s %8.2f\n", "GK (2008): 2006-2023 forecast period",  gk_oos_r2))
cat("\n")

# ======================================================================
# TABLE 4: SHAPLEY VALUE VARIANCE DECOMPOSITION (GK3)
# ======================================================================
cat("======================================================================\n")
cat("TABLE 4: SHAPLEY VALUES FOR GOLDIN & KATZ'S PREFERRED SPECIFICATION\n")
cat("======================================================================\n\n")

set.seed(1992)
n_boot <- 1000
boot_res <- matrix(NA, nrow = n_boot, ncol = 3)
colnames(boot_res) <- c("Supply", "TimeTrend", "TimeBreaks")

for(i in 1:n_boot) {
  valid <- FALSE
  while(!valid) {
    b_data <- gk_train[sample(nrow(gk_train), replace = TRUE), ]
    if(var(b_data$dummy_1949) > 0) {
      b_mod <- lm(wprem2 ~ relsup + time + time_post59 + time_post92 + dummy_1949, data = b_data)
      if(!any(is.na(coef(b_mod)))) valid <- TRUE
    }
  }
  capture.output(relimp <- calc.relimp(b_mod, type = "lmg", rela = FALSE))
  boot_res[i, "Supply"] <- relimp$lmg["relsup"]
  boot_res[i, "TimeTrend"] <- relimp$lmg["time"]
  boot_res[i, "TimeBreaks"] <- sum(relimp$lmg[c("time_post59", "time_post92", "dummy_1949")])
}

capture.output(full_relimp <- calc.relimp(gk3, type = "lmg", rela = FALSE))
pe_s <- full_relimp$lmg["relsup"]
pe_t <- full_relimp$lmg["time"]
pe_b <- sum(full_relimp$lmg[c("time_post59", "time_post92", "dummy_1949")])

ci_s <- quantile(boot_res[, "Supply"], probs = c(0.025, 0.975))
ci_t <- quantile(boot_res[, "TimeTrend"], probs = c(0.025, 0.975))
ci_b <- quantile(boot_res[, "TimeBreaks"], probs = c(0.025, 0.975))

cat(sprintf("%-25s %.3f\n", "Log relative supply", pe_s))
cat(sprintf("%-25s (%.3f, %.3f)\n", "", ci_s[1], ci_s[2]))
cat(sprintf("%-25s %.3f\n", "Time trend", pe_t))
cat(sprintf("%-25s (%.3f, %.3f)\n", "", ci_t[1], ci_t[2]))
cat(sprintf("%-25s %.3f\n", "Time breaks", pe_b))
cat(sprintf("%-25s (%.3f, %.3f)\n", "", ci_b[1], ci_b[2]))
cat("\nNote: 95% bootstrap confidence intervals from 1,000 resampling-with-replacement iterations.\n")
cat("Bootstrap is conditional: any sample in which the 1949 observation is absent\n")
cat("(i.e., dummy_1949 has zero variance) is discarded and redrawn, so the effective\n")
cat("resampling scheme is conditional on 1949 always being represented.\n\n")

# ======================================================================
# ORIGINAL MISSPECIFICATION: 1963-1987 TESTS
# ======================================================================
cat("======================================================================\n")
cat("UNIT ROOT & COINTEGRATION TESTS: KATZ & MURPHY ORIGINAL PERIOD (1963-1987)\n")
cat("======================================================================\n\n")

# ADF Tests on Levels (1963-1987)
adf_wprem_87 <- ur.df(km_data$wprem, type = "trend", selectlags = "AIC")
adf_relsup_87 <- ur.df(km_data$relsup, type = "trend", selectlags = "AIC")

# Engle-Granger Cointegration Test (1963-1987)
km_coint_model <- lm(wprem ~ relsup + time, data = km_data)
eg_adf_87 <- ur.df(resid(km_coint_model), type = "none", selectlags = "AIC")

cat(sprintf("ADF Test for Unit Root (1963-1987)\n"))
cat(sprintf("Log wage premium statistic:      %.3f\n", adf_wprem_87@teststat[1]))
cat(sprintf("Log relative supply statistic:   %.3f\n\n", adf_relsup_87@teststat[1]))

cat(sprintf("Engle-Granger Cointegration Test (1963-1987)\n"))
cat(sprintf("ADF on residuals statistic:      %.3f\n\n", eg_adf_87@teststat[1]))

# ======================================================================
# TABLE 5: UNIT ROOT TESTS, 1963-2023
# ======================================================================
cat("======================================================================\n")
cat("TABLE 5: UNIT ROOT TESTS, 1963-2023\n")
cat("======================================================================\n\n")

# Helper function for Zivot-Andrews with automatic lag selection (AIC).
# Note: models with higher lags use fewer observations, making AIC values
# not strictly comparable across lag lengths. In practice the bias is
# limited here: max lag 8 on a T=61 series is a ≤13% sample variation.
ur.za.auto <- function(x, model = "both", maxlag = 8) {
  best_aic <- Inf
  best_za <- NULL
  best_lag <- 0L
  for (l in 0:maxlag) {
    # ur.za uses the full regression for its testreg slot, which contains AIC
    curr_za <- ur.za(x, model = model, lag = l)
    # Extract AIC from the test regression at the optimal break point
    curr_aic <- AIC(curr_za@testreg)
    if (curr_aic < best_aic) {
      best_aic <- curr_aic
      best_za <- curr_za
      best_lag <- l
    }
  }
  # Store the selected lag as an attribute (the ur.za S4 class does not have a
  # writable @lag slot, so we use attr() instead).
  attr(best_za, "selected_lag") <- best_lag
  return(best_za)
}

run_ur_tests <- function(x) {
  list(
    adf = ur.df(x, type = "trend", selectlags = "AIC"),
    pp = ur.pp(x, type = "Z-tau", model = "trend", lags = "short"),
    kpss = ur.kpss(x, type = "tau", lags = "short"),
    za = ur.za.auto(x, model = "both")
  )
}

ur_wprem <- run_ur_tests(ts_data$wprem2)
ur_relsup <- run_ur_tests(ts_data$relsup)

print_ur_table <- function(ur_res, title) {
  cat(sprintf("%s\n", title))
  cat(sprintf("%-10s %-15s %-15s %-10s %-10s %-10s\n", "Test", "Lags/bw", "Statistic", "1%", "5%", "10%"))
  cat(strrep("-", 75), "\n")
  cat(sprintf("%-10s %-15s %-15.2f %-10.2f %-10.2f %-10.2f\n", 
              "ADF", paste(ur_res$adf@lags, "lag"), ur_res$adf@teststat[1], ur_res$adf@cval[1,1], ur_res$adf@cval[1,2], ur_res$adf@cval[1,3]))
  cat(sprintf("%-10s %-15s %-15.2f %-10.2f %-10.2f %-10.2f\n", 
              "PP", "Short", ur_res$pp@teststat[1], ur_res$pp@cval[1,1], ur_res$pp@cval[1,2], ur_res$pp@cval[1,3]))
  # ur.kpss stores @cval in order 10%, 5%, 2.5%, 1% — use [4],[2],[1] for columns 1%/5%/10%
  cat(sprintf("%-10s %-15s %-15.2f %-10.2f %-10.2f %-10.2f\n",
              "KPSS", "Short", ur_res$kpss@teststat[1], ur_res$kpss@cval[4], ur_res$kpss@cval[2], ur_res$kpss@cval[1]))
  za_lag <- attr(ur_res$za, "selected_lag")
  cat(sprintf("%-10s %-15s %-15.2f %-10.2f %-10.2f %-10.2f\n",
              "ZA", paste(za_lag, "lag"), ur_res$za@teststat[1], ur_res$za@cval[1], ur_res$za@cval[2], ur_res$za@cval[3]))
  cat("\n")
}

print_ur_table(ur_wprem, "(a) Log wage premium")
print_ur_table(ur_relsup, "(b) Log relative supply")

# Zivot-Andrews endogenously identified break dates
za_break_wprem  <- ts_data$year[ur_wprem$za@bpoint]
za_break_relsup <- ts_data$year[ur_relsup$za@bpoint]
cat(sprintf("Zivot-Andrews break dates: wage premium = %d, relative supply = %d\n\n",
            za_break_wprem, za_break_relsup))


# ======================================================================
# TABLE 6: COINTEGRATION TESTS, 1963-2023
# ======================================================================
cat("======================================================================\n")
cat("TABLE 6: COINTEGRATION TESTS, 1963-2023\n")
cat("======================================================================\n\n")

# Johansen: select VAR lag order by AIC, with a minimum of 2 lags to ensure
# adequate dynamics.  If AIC selects fewer than 2 lags, the floor of 2 applies.
var_sel <- VARselect(ts_data[, c("wprem2", "relsup")], lag.max = 4, type = "both")
aic_k <- var_sel$selection["AIC(n)"]
jo_k <- max(2L, aic_k)
cat(sprintf("VAR lag order: AIC selected K = %d, using K = %d (minimum 2)\n\n", aic_k, jo_k))
jo_test <- ca.jo(ts_data[, c("wprem2", "relsup")], type = "trace", ecdet = "trend", K = jo_k)

# Cointegrating regression (OLS) — used for Engle-Granger residuals
coint_model <- lm(wprem2 ~ relsup + year, data = ts_data)
eg_adf <- ur.df(resid(coint_model), type = "none", selectlags = "AIC")

# DOLS cointegrating regression (1 lead + 1 lag of Δrelsup) — used for Shin residuals.
# Shin (1994) specified the test using OLS residuals from a static cointegrating
# regression, so OLS is valid. DOLS is used here for improved finite-sample properties
# (corrects for serial correlation and endogeneity in the cointegrating regression);
# it is standard practice but not required for the test to be asymptotically valid.
ts_data_dols <- ts_data %>%
  mutate(d_relsup       = c(NA, diff(relsup)),
         d_relsup_lag1  = lag(d_relsup, 1),
         d_relsup_lead1 = lead(d_relsup, 1))
coint_model_dols <- lm(wprem2 ~ relsup + year + d_relsup_lag1 + d_relsup_lead1,
                        data = ts_data_dols)
shin_test <- ur.kpss(resid(coint_model_dols), type = "mu", lags = "short")
# Shin (1994) cointegration test critical values for m=1 stochastic regressor
# plus a deterministic trend (Table 1 in Shin 1994).  The ur.kpss function
# returns standard univariate KPSS critical values, which are larger and would
# incorrectly favour the null of cointegration.  We override them here.
shin_cv <- c("10pct" = 0.097, "5pct" = 0.121, "2.5pct" = 0.146, "1pct" = 0.184)

# Gregory-Hansen equivalent via Maki (2012) test with m=1 break.
# Model 3 in makicoint is "Trend and Regime Shift" (GH Model 4).
gh_maki <- coint_maki(as.matrix(ts_data[, c("wprem2", "relsup")]), m = 1, model = 3)
gh_break_year <- ts_data$year[gh_maki$breakpoints[1]]
# Assign post95 dynamically from the Maki-identified endogenous break year
ts_data$post95 <- as.numeric(ts_data$year > gh_break_year)

cat(sprintf("%-25s %-15s %-10s %-10s %-10s\n", "Test", "Statistic", "1%", "5%", "10%"))
cat(strrep("-", 75), "\n")
cat("Johansen\n")
cat(sprintf("  %-23s %-15.2f %-10.2f %-10.2f %-10.2f\n", "r <= 0", jo_test@teststat[2], jo_test@cval[2,3], jo_test@cval[2,2], jo_test@cval[2,1]))
cat(sprintf("  %-23s %-15.2f %-10.2f %-10.2f %-10.2f\n", "r <= 1", jo_test@teststat[1], jo_test@cval[1,3], jo_test@cval[1,2], jo_test@cval[1,1]))
# MacKinnon (1991) response-surface critical values for 2-variable EG test,
# constant and trend in cointegrating regression (matches lm(wprem2 ~ relsup + year)),
# T=61. Standard ADF cval are invalid here.
eg_cv_1pct  <- -4.26
eg_cv_5pct  <- -3.63
eg_cv_10pct <- -3.34
cat(sprintf("%-25s %-15.2f %-10.2f %-10.2f %-10.2f\n", "Engle-Granger (ADF)", eg_adf@teststat[1], eg_cv_1pct, eg_cv_5pct, eg_cv_10pct))
cat(sprintf("%-25s %-15.2f %-10.3f %-10.3f %-10.3f\n", "Shin (KPSS)", shin_test@teststat[1], shin_cv["1pct"], shin_cv["5pct"], shin_cv["10pct"]))
cat(sprintf("%-25s %-15.2f %-10.2f %-10.2f %-10.2f (Break: %d)\n", "Gregory-Hansen (ADF*)", gh_maki$statistic, gh_maki$critical_values[1], gh_maki$critical_values[2], gh_maki$critical_values[3], gh_break_year))
cat("\n")

# ======================================================================
# TABLE 7: FIRST-DIFFERENCE REGRESSIONS, 1963-2023
# ======================================================================
# Given the uniform rejection of cointegration (Tables 5-6), the appropriate
# specification for non-cointegrated I(1) variables is in first differences.
# This table is the primary time-series result; the ECM in Table 8 follows
# as a robustness check that meets the KM framework on its own terms.
cat("======================================================================\n")
cat("TABLE 7: FIRST-DIFFERENCE REGRESSIONS, 1963-2023\n")
cat("======================================================================\n\n")

fd_data <- ts_data %>%
  arrange(year) %>%
  mutate(
    d_wprem  = c(NA, diff(wprem2)),
    d_relsup = c(NA, diff(relsup)),
    d_post92 = c(NA, diff(post92)),
    d_post95 = c(NA, diff(post95))
  ) %>%
  filter(!is.na(d_wprem))

fd_data <- fd_data %>%
  mutate(
    l1_d_wprem  = lag(d_wprem, 1),
    l1_d_relsup = lag(d_relsup, 1)
  )
# Col (1): contemporaneous effect only
fd_1 <- lm(d_wprem ~ d_relsup, data = fd_data)
# Col (2): + one lag of each differenced variable (ADL(1,1) in differences)
fd_2 <- lm(d_wprem ~ d_relsup + l1_d_wprem + l1_d_relsup, data = fd_data)

# Newey-West HAC standard errors (lag = floor(0.75 * T^(1/3)))
nw_lag <- floor(0.75 * nrow(fd_data)^(1/3))
fd_models <- list(fd_1, fd_2)
fd_labels <- c("(1)", "(2)")

fd_nw_se <- lapply(fd_models, function(m) {
  sqrt(diag(sandwich::NeweyWest(m, lag = nw_lag, prewhite = FALSE)))
})
fd_nw_pval <- lapply(seq_along(fd_models), function(i) {
  cf <- coef(fd_models[[i]])
  se <- fd_nw_se[[i]]
  2 * pt(abs(cf / se), df = df.residual(fd_models[[i]]), lower.tail = FALSE)
})

cat(sprintf("%-30s %-15s %-15s\n", "", fd_labels[1], fd_labels[2]))
cat(strrep("-", 30 + 15 * 3), "\n")

fd_print_row <- function(label, varname) {
  cat(sprintf("%-30s ", label))
  for (i in seq_along(fd_models)) {
    cf <- coef(fd_models[[i]])
    if (varname %in% names(cf)) {
      est <- cf[varname]
      pval <- fd_nw_pval[[i]][varname]
      cat(sprintf("%-15s ", paste0(sprintf("%.4f", est), add_stars(pval))))
    } else {
      cat(sprintf("%-15s ", ""))
    }
  }
  cat("\n")
  cat(sprintf("%-30s ", ""))
  for (i in seq_along(fd_models)) {
    cf <- coef(fd_models[[i]])
    if (varname %in% names(cf)) {
      se <- fd_nw_se[[i]][varname]
      cat(sprintf("%-15s ", paste0("(", sprintf("%.4f", se), ")")))
    } else {
      cat(sprintf("%-15s ", ""))
    }
  }
  cat("\n")
}

fd_print_row("Delta log relative supply", "d_relsup")
fd_print_row("Delta log wage premium (-1)", "l1_d_wprem")
fd_print_row("Delta log rel supply (-1)", "l1_d_relsup")
fd_print_row("Constant", "(Intercept)")

cat(strrep("-", 30 + 15 * 3), "\n")
cat(sprintf("%-30s ", "Observations"))
for (m in fd_models) cat(sprintf("%-15d ", nobs(m)))
cat("\n")
cat(sprintf("%-30s ", "Adjusted R-squared"))
for (m in fd_models) cat(sprintf("%-15.3f ", summary(m)$adj.r.squared))
cat("\n")
cat(sprintf("%-30s %s\n", "HAC standard errors", paste0("Newey-West (", nw_lag, " lags)")))
cat("Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

# ======================================================================
# TABLE 8: ERROR CORRECTION MODEL (ROBUSTNESS), 1963-2023
# ======================================================================
# The ARDL/ECM bounds test (Pesaran, Shin & Smith 2001) does not require
# pre-testing for unit roots and is valid whether variables are I(0) or I(1).
# It is presented here as a robustness check: even if one assumes a levels
# relationship exists (as KM implicitly do), the bounds test fails to find one.
cat("======================================================================\n")
cat("TABLE 8: ERROR CORRECTION MODEL (ROBUSTNESS), 1963-2023\n")
cat("======================================================================\n\n")

# Col (1): supply only (bivariate test — no trend, no break)
ardl_1 <- auto_ardl(wprem2 ~ relsup, data = ts_data, max_order = c(2, 2), starting_order = c(1, 0), grid = TRUE)
ecm_1 <- uecm(ardl_1$best_model)
# Col (2): + time trend, no break (KM specification)
ardl_2 <- auto_ardl(wprem2 ~ relsup + time, data = ts_data, max_order = c(2, 2, 0), starting_order = c(1, 0, 0), grid = TRUE)
ecm_2 <- uecm(ardl_2$best_model)
# Col (3): + time trend + post-1992 level shift (AKK/GK specification).
# Note: the OLS models (Tables 2-3) use time_post92 (slope change), while
# this ARDL uses post92 (level shift). These test different relationships;
# the level-shift specification is consistent with the ECM framework here.
ardl_3 <- auto_ardl(wprem2 ~ relsup + time + post92, data = ts_data, max_order = c(2, 2, 0, 0), starting_order = c(1, 0, 0, 0), grid = TRUE)
ecm_3 <- uecm(ardl_3$best_model)
# Col (4): + time trend + post-1995 break (Gregory-Hansen identified break year)
ardl_4 <- auto_ardl(wprem2 ~ relsup + time + post95, data = ts_data, max_order = c(2, 2, 0, 0), starting_order = c(1, 0, 0, 0), grid = TRUE)
ecm_4 <- uecm(ardl_4$best_model)
ecm_list <- list(ecm_1, ecm_2, ecm_3, ecm_4)
ardl_list <- list(ardl_1, ardl_2, ardl_3, ardl_4)

# Case 3 (unrestricted intercept, no trend) is used for Col (1) and as the
# primary specification for all columns. For Cols (2)-(4), which include a
# deterministic time trend, PSS (2001) Case 5 (unrestricted intercept and
# trend) is also computed below as a robustness check.
set.seed(1992)
b_tests <- lapply(ecm_list, function(e) bounds_f_test(e, case = 3, alpha = 0.05, exact = TRUE, R = 40000))
# Case 5 robustness for trended specifications (cols 2-4).
# bounds_f_test(case=5) requires the ARDL model to contain an explicit trend
# term recognised by the package. If the trend is entered as a plain regressor
# (as here), the package will reject case=5. We attempt it and fall back
# gracefully if unsupported.
set.seed(1992)
b_tests_c5 <- lapply(ecm_list[2:4], function(e) {
  tryCatch(bounds_f_test(e, case = 5, alpha = 0.05, exact = TRUE, R = 40000),
           error = function(err) list(statistic = NA_real_, note = err$message))
})

cat(sprintf("%-30s %-15s %-15s %-15s %-15s\n", "", "(1)", "(2)", "(3)", "(4)"))
cat(strrep("-", 106), "\n")
cat("(a) Bounds test (F-statistic)\n")
cat(sprintf("%-30s", "F-statistic"))
for (b in b_tests) cat(sprintf("%-15.3f ", b$statistic))
cat("\n")

local({
  bcvs <- b_tests[[1]]$critical_values
  # k counts only stochastic I(1) regressors (PSS 2001). post92 and post95 are
  # stationary I(0) dummies and do not contribute to k. k=1 (relsup only) throughout.
  cat("Critical values [I(0)/I(1)], PSS (2001) Case 3 asymptotic (k=1 for all columns):\n")
  cat(sprintf("  %-28s %-15s\n", "10%", "[3.84, 4.58]"))
  cat(sprintf("  %-28s %-15s\n", "5%",  "[4.94, 5.73]"))
  cat(sprintf("  %-28s %-15s\n", "1%",  "[7.84, 8.82]"))
})

if (any(!is.na(sapply(b_tests_c5, `[[`, "statistic")))) {
  cat("\nRobustness: Case 5 F-statistics for trended specifications (Cols 2-4):\n")
  cat(sprintf("%-30s", "F-statistic (Case 5)"))
  cat(sprintf("%-15s ", "---"))  # Col 1 not applicable
  for (b in b_tests_c5) {
    if (is.na(b$statistic)) cat(sprintf("%-15s ", "N/A"))
    else cat(sprintf("%-15.3f ", b$statistic))
  }
  cat("\n")
} else {
  cat("\nNote: Case 5 bounds test could not be computed — the ARDL package requires\n")
  cat("the trend to be specified via the package's own trend mechanism, not as a\n")
  cat("plain regressor. Case 3 results are reported above; Case 5 critical values\n")
  cat("(PSS 2001 Table CI(v)) are wider, so Case 3 is the more conservative test.\n")
}

cat("\n(b) Long-run coefficients\n")
lr_list <- lapply(ecm_list, function(e) multipliers(e, type = "lr"))

lr_val <- function(lr, term) {
  v <- lr$Estimate[lr$Term == term]
  if (length(v) == 0) NA_real_ else v
}
lr_se <- function(lr, term) {
  v <- lr$`Std. Error`[lr$Term == term]
  if (length(v) == 0) NA_real_ else v
}
lr_pval <- function(lr, term) {
  col_nm <- grep("Pr\\(", names(lr), value = TRUE)
  if (length(col_nm) == 0) return(NA_real_)
  v <- lr[[col_nm[1]]][lr$Term == term]
  if (length(v) == 0) NA_real_ else v
}
fmt_lr   <- function(val, pval = NA) {
  if (is.na(val)) return(sprintf("%-15s ", "---"))
  stars <- if (is.na(pval)) "" else add_stars(pval)
  sprintf("%-15s ", paste0(sprintf("%.3f", val), stars))
}
fmt_lrse <- function(x) if (is.na(x)) sprintf("%-15s ", "---") else sprintf("(%.3f)         ", x)

print_lr_row <- function(label, term) {
  cat(sprintf("%-30s ", label))
  for (lr in lr_list) cat(fmt_lr(lr_val(lr, term), lr_pval(lr, term)))
  cat("\n")
  cat(sprintf("%-30s ", ""))
  for (lr in lr_list) cat(fmt_lrse(lr_se(lr, term)))
  cat("\n")
}

print_lr_row("Log relative supply", "relsup")
print_lr_row("Time trend", "time")
print_lr_row("Post-1992 dummy", "post92")
print_lr_row("Post-1995 dummy", "post95")
print_lr_row("Constant", "(Intercept)")

cat("\nPanel (c): Error correction estimates\n")
ecm_coef_list <- lapply(ecm_list, function(e) summary(e)$coefficients)

ecm_row <- function(coef_mat, pattern, col = 1) {
  nm <- grep(pattern, rownames(coef_mat), value = TRUE)
  if (length(nm) == 0) return(NA_real_)
  coef_mat[nm[1], col]
}
fmt_coef <- function(val, pval = NA) {
  if (is.na(val)) return(sprintf("%-15s ", "---"))
  stars <- if (is.na(pval)) "" else add_stars(pval)
  sprintf("%-15s ", paste0(sprintf("%.3f", val), stars))
}
fmt_se   <- function(x) if (is.na(x)) sprintf("%-15s ", "---") else sprintf("(%.3f)         ", x)

ecm_print_row <- function(label, pattern) {
  cat(sprintf("%-30s ", label))
  for (m in ecm_coef_list) cat(fmt_coef(ecm_row(m, pattern), ecm_row(m, pattern, 4)))
  cat("\n")
  cat(sprintf("%-30s ", ""))
  for (m in ecm_coef_list) cat(fmt_se(ecm_row(m, pattern, 2)))
  cat("\n")
}

ecm_print_row("Log wage premium (-1)",   "L\\(wprem2")
ecm_print_row("Time trend",              "^time")
ecm_print_row("Structural break dummy",  "post")
ecm_print_row("Constant",               "Intercept")

cat("\n")
cat(sprintf("%-30s ", "Observations"))
for (m in ecm_list) cat(sprintf("%-15d ", nobs(m)))
cat("\n")
cat(sprintf("%-30s ", "ARDL order (p, q)"))
for (a in ardl_list) {
  ord <- a$best_order
  cat(sprintf("%-15s ", paste0("(", paste(ord, collapse=","), ")")))
}
cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n")


sink()

cat(paste(readLines(output_file), collapse = "\n"))
cat(paste0("\n\nResults saved to: ", output_file, "\n"))
cat("Plots saved: KM_1992_Prediction.pdf, AKK_2008_Prediction.pdf, GK_2008_Prediction.pdf\n")