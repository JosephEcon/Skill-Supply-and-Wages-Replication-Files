# ==============================================================================
#  task clustering.R
#  Purpose: ALTERNATIVE clustering of the 175 college majors based on the WORK
#           ACTIVITIES their graduates' occupations require (O*NET), instead of
#           on the occupation distribution directly.
#
#           For each major, the activity score is the proportion of its
#           graduates whose occupation requires activity a. Equivalently
#               M = S %*% A
#           where S (major x occupation share) is built by degree_soc6_shares.do
#           and A (occupation x activity, binary "requires") by onet_activity_prep.R.
#
#           Builds M at three O*NET granularities (GWA / IWA / DWA), clusters with
#           weighted hierarchical clustering (members = totalcellsize) under
#           cosine and Manhattan distance and average / Ward linkage, reports
#           internal validation (ASWw / HC / HG / cophenetic correlation), and
#           compares the resulting partitions against the current occupation-based
#           clusters (weighted_average_linkage_jsd_soc4.dta) via Adjusted Rand
#           Index, NMI and a k=25 cross-tabulation.
#
#  Inputs : Data/intermediate/degree_soc6_shares.dta, degree_totalcellsize.dta,
#           onet_requires_{gwa,iwa,dwa}.dta, onet_labels_{gwa,iwa,dwa}.dta,
#           weighted_average_linkage_jsd_soc4.dta  (occupation reference partition)
#  Outputs: Data/intermediate/task_weighted_{link}_{dist}_{level}.dta
#           Output/task cluster validation {level} {dist} {link}.jpeg
#           Output/task_internal_validation.csv      (ASWw/HC/HG peak-k + cophenetic)
#           Output/task_vs_occ_agreement.csv         (ARI/NMI by k, every config)
#           Output/task_vs_occ_agreement.png         (headline ARI/NMI vs k)
#           Output/task_vs_occ_crosstab_k25.csv      (headline cross-tab)
#           Output/task_vs_occ_majors_k25.csv        (per-major occ vs task cluster)
# ==============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && !dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

for (pkg in c("haven", "dplyr", "cluster", "fastcluster", "WeightedCluster")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
suppressMessages({
  library(haven); library(dplyr); library(cluster)
  library(fastcluster); library(WeightedCluster)
})

# ---- paths ----
mypath <- "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
path_intermediatedata <- file.path(mypath, "Data", "intermediate")
path_output           <- file.path(mypath, "Output")

LEVELS <- c("gwa", "iwa", "dwa")
DISTS  <- c("cosine", "manhattan")
LINKS  <- c("average", "ward.D2")
KS_CMP <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100)   # ks present in reference file (<175)
KS_SAVE<- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
KMAX_V <- 50
HEADLINE <- list(level = "gwa", dist = "cosine", link = "average")

link_name <- function(l) if (l == "average") "average_linkage" else "ward_linkage"

# ==============================================================================
#  Helper functions
# ==============================================================================

# cosine distance on rows of M (1 - cosine similarity)
cosine_dist <- function(M) {
  nrm <- sqrt(rowSums(M^2)); nrm[nrm == 0] <- 1
  sim <- (M %*% t(M)) / (nrm %o% nrm)
  sim[sim > 1] <- 1; sim[sim < -1] <- -1
  as.dist(1 - sim)
}

# Adjusted Rand Index from two label vectors
adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  comb2 <- function(x) sum(choose(x, 2))
  ai <- rowSums(tab); bj <- colSums(tab); n <- sum(tab)
  idx <- comb2(as.vector(tab))
  exp <- comb2(ai) * comb2(bj) / choose(n, 2)
  mx  <- (comb2(ai) + comb2(bj)) / 2
  if (mx - exp == 0) return(1)
  (idx - exp) / (mx - exp)
}

# Normalized Mutual Information (sqrt normalization)
nmi <- function(a, b) {
  tab <- as.matrix(table(a, b)); n <- sum(tab)
  pij <- tab / n; pi_ <- rowSums(pij); pj <- colSums(pij)
  nz  <- pij > 0
  MI  <- sum(pij[nz] * log(pij[nz] / outer(pi_, pj)[nz]))
  Hx  <- -sum(pi_[pi_ > 0] * log(pi_[pi_ > 0]))
  Hy  <- -sum(pj[pj  > 0] * log(pj[pj  > 0]))
  if (Hx <= 0 || Hy <= 0) return(NA_real_)
  MI / sqrt(Hx * Hy)
}

# Build an activity profile for every ACS occsoc by matching to the O*NET soc6
# matrix. Exact match where possible; otherwise aggregate (mean) over all O*NET
# soc6 sharing the longest known prefix. Handles IPUMS "X" placeholders (e.g.
# 1110XX -> known prefix "1110") and trailing-zero broad SOC codes.
build_A_acs <- function(acs_soc6, A_onet) {
  onet_soc6 <- rownames(A_onet)
  acts <- colnames(A_onet)
  out  <- matrix(0, nrow = length(acs_soc6), ncol = length(acts),
                 dimnames = list(acs_soc6, acts))
  mtype <- character(length(acs_soc6))
  for (i in seq_along(acs_soc6)) {
    code <- acs_soc6[i]
    if (code %in% onet_soc6) { out[i, ] <- A_onet[code, ]; mtype[i] <- "exact"; next }
    key <- gsub("X", "", code, fixed = TRUE)          # leading known digits
    matched <- FALSE
    for (p in seq(nchar(key), 2)) {
      sel <- startsWith(onet_soc6, substr(key, 1, p))
      if (any(sel)) {
        out[i, ] <- colMeans(A_onet[sel, , drop = FALSE])
        mtype[i] <- paste0("prefix", p); matched <- TRUE; break
      }
    }
    if (!matched) mtype[i] <- "none"
  }
  list(A = out, mtype = mtype)
}

# ==============================================================================
#  Load shared inputs
# ==============================================================================
cat("Loading inputs ...\n")
shares <- read_dta(file.path(path_intermediatedata, "degree_soc6_shares.dta"))
shares$degfieldd <- as.character(shares$degfieldd)
S <- as.matrix(unclass(xtabs(share ~ degfieldd + soc6, data = shares)))

tcs <- read_dta(file.path(path_intermediatedata, "degree_totalcellsize.dta"))
deg_label <- as.character(haven::as_factor(tcs$degfieldd))
names(deg_label) <- as.character(tcs$degfieldd)
weights <- tcs$totalcellsize[match(rownames(S), as.character(tcs$degfieldd))]
names(weights) <- rownames(S)

ref <- read_dta(file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc4.dta"))
ref$degfieldd <- as.character(ref$degfieldd)

cat(sprintf("  S: %d majors x %d ACS occupations (soc6)\n", nrow(S), ncol(S)))

# occupation graduate-weight, for match-rate reporting
occ_w <- as.vector(t(S) %*% weights); names(occ_w) <- colnames(S)

# accumulators
agree_rows <- list()
internal_rows <- list()

# ==============================================================================
#  Main loop over levels x distances x linkages
# ==============================================================================
for (level in LEVELS) {

  edges  <- read_dta(file.path(path_intermediatedata, sprintf("onet_requires_%s.dta", level)))
  A_onet <- as.matrix(unclass(xtabs(requires ~ soc6 + activity_id, data = edges)))

  mm <- build_A_acs(colnames(S), A_onet)
  A_acs <- mm$A
  matched <- mm$mtype != "none"

  any_rate   <- sum(occ_w[matched]) / sum(occ_w)
  exact_rate <- sum(occ_w[mm$mtype == "exact"]) / sum(occ_w)
  cat(sprintf("\n[%s] occupation match (graduate-weighted): exact=%.3f  any=%.3f  (%d/%d distinct occ unmatched)\n",
              toupper(level), exact_rate, any_rate, sum(!matched), length(matched)))

  # M = proportion of a major's (matched) graduates whose occupation requires each activity
  S_m <- S[, matched, drop = FALSE]
  M   <- S_m %*% A_acs[matched, , drop = FALSE]
  M   <- M / rowSums(S_m)                       # renormalize over matched grads
  M   <- M[, colSums(M) > 0, drop = FALSE]      # drop never-required activities
  cat(sprintf("[%s] task matrix M: %d majors x %d activities; range [%.3f, %.3f]\n",
              toupper(level), nrow(M), ncol(M), min(M), max(M)))

  for (dist_method in DISTS) {
    dist_obj <- if (dist_method == "cosine") cosine_dist(M) else dist(M, method = "manhattan")

    for (linkage in LINKS) {
      hc <- hclust(dist_obj, method = linkage, members = weights)

      # ---- internal validation across k = 2..KMAX_V ----
      kk <- 2:KMAX_V
      ASWw <- HC <- HG <- numeric(length(kk))
      for (i in seq_along(kk)) {
        cl <- cutree(hc, k = kk[i])
        st <- wcClusterQuality(diss = dist_obj, cluster = cl, weights = weights)$stats
        ASWw[i] <- st["ASWw"]; HC[i] <- st["HC"]; HG[i] <- st["HG"]
      }
      cophen <- as.numeric(cor(cophenetic(hc), dist_obj))

      jpeg(file.path(path_output, sprintf("task cluster validation %s %s %s.jpeg",
                                          level, dist_method, sub("\\..*", "", linkage))),
           width = 800, height = 1200, res = 150)
      op <- par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
      plot(kk, ASWw, type = "b", pch = 20, xlab = "Number of clusters", ylab = "ASWw",
           main = sprintf("%s | %s | %s  (cophenetic r = %.2f)\nWeighted Avg Silhouette (ASWw) - maximise",
                          toupper(level), dist_method, sub("\\..*", "", linkage), cophen))
      plot(kk, HC, type = "b", pch = 20, xlab = "Number of clusters", ylab = "Hubert's C",
           main = "Hubert's C - minimise")
      plot(kk, HG, type = "b", pch = 20, xlab = "Number of clusters", ylab = "Hubert's Gamma",
           main = "Hubert's Gamma - maximise")
      par(op); dev.off()

      internal_rows[[length(internal_rows) + 1]] <- data.frame(
        level = level, distance = dist_method, linkage = sub("\\..*", "", linkage),
        best_k_ASWw = kk[which.max(ASWw)], max_ASWw = max(ASWw),
        best_k_HG = kk[which.max(HG)], cophenetic_r = cophen,
        n_activities = ncol(M), stringsAsFactors = FALSE)

      # ---- save cluster assignments ----
      out <- data.frame(degfieldd = rownames(M), stringsAsFactors = FALSE)
      for (k in KS_SAVE) out[[paste0("cluster", k)]] <- cutree(hc, k = k)[out$degfieldd]
      write_dta(out, file.path(path_intermediatedata,
                               sprintf("task_weighted_%s_%s_%s.dta", link_name(linkage), dist_method, level)))

      # ---- agreement vs occupation reference partition ----
      common <- intersect(rownames(M), ref$degfieldd)
      ref_aln <- ref[match(common, ref$degfieldd), ]
      for (k in KS_CMP) {
        kc <- paste0("cluster", k)
        if (!kc %in% names(ref)) next
        task_cl <- cutree(hc, k = k)[common]
        occ_cl  <- ref_aln[[kc]]
        agree_rows[[length(agree_rows) + 1]] <- data.frame(
          level = level, distance = dist_method, linkage = sub("\\..*", "", linkage),
          k = k, ARI = adjusted_rand(occ_cl, task_cl), NMI = nmi(occ_cl, task_cl),
          stringsAsFactors = FALSE)
      }

      # ---- headline k=25 cross-tab + per-major mapping ----
      if (level == HEADLINE$level && dist_method == HEADLINE$dist && linkage == HEADLINE$link) {
        task25 <- cutree(hc, k = 25)[common]
        occ25  <- ref_aln[["cluster25"]]
        ct <- table(occ_cluster = occ25, task_cluster = task25)
        write.csv(as.data.frame.matrix(ct),
                  file.path(path_output, "task_vs_occ_crosstab_k25.csv"))
        maj <- data.frame(degfieldd = common, major = deg_label[common],
                          occ_cluster25 = occ25, task_cluster25 = task25,
                          stringsAsFactors = FALSE)
        maj <- maj[order(maj$task_cluster25, maj$occ_cluster25), ]
        write.csv(maj, file.path(path_output, "task_vs_occ_majors_k25.csv"), row.names = FALSE)
      }
    }
  }
}

# ==============================================================================
#  Write summary tables + headline plot
# ==============================================================================
internal <- do.call(rbind, internal_rows)
agree    <- do.call(rbind, agree_rows)
write.csv(internal, file.path(path_output, "task_internal_validation.csv"), row.names = FALSE)
write.csv(agree,    file.path(path_output, "task_vs_occ_agreement.csv"),    row.names = FALSE)

# headline plot: ARI & NMI vs k for the 3 levels under cosine + average linkage
hp <- agree %>% filter(distance == "cosine", linkage == "average")
png(file.path(path_output, "task_vs_occ_agreement.png"), width = 1100, height = 550, res = 120)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
cols <- c(gwa = "#1b9e77", iwa = "#d95f02", dwa = "#7570b3")
for (metric in c("ARI", "NMI")) {
  plot(NA, xlim = range(KS_CMP), ylim = c(0, max(0.6, max(hp[[metric]], na.rm = TRUE))),
       xlab = "Number of clusters (k)", ylab = metric,
       main = sprintf("%s: task-based vs occupation-based\n(cosine, average linkage)", metric))
  for (lv in LEVELS) {
    d <- hp[hp$level == lv, ]; d <- d[order(d$k), ]
    lines(d$k, d[[metric]], type = "b", pch = 20, col = cols[lv], lwd = 2)
  }
  legend("topright", legend = toupper(LEVELS), col = cols[LEVELS], lwd = 2, pch = 20, bty = "n")
}
par(op); dev.off()

cat("\n================ INTERNAL VALIDATION (per config) ================\n")
print(internal, row.names = FALSE)
cat("\n================ AGREEMENT vs OCCUPATION CLUSTERS ================\n")
print(agree[order(agree$level, agree$distance, agree$linkage, agree$k), ], row.names = FALSE)
cat("\ntask clustering.R complete.\n")
