# --- ensure CRAN mirror and user library ---
options(repos = c(CRAN = "https://cloud.r-project.org"))    # set mirror for non-interactive installs
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && !dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))                         # prefer user library to avoid admin rights


# ─────────────────────────────────────────────────────────────────────────────
# LIBRARIES & PATHS
# ─────────────────────────────────────────────────────────────────────────────

library(haven)
library(dplyr)
library(cluster)
library(fastcluster)       # optional: faster hclust implementation
library(WeightedCluster)   # for weighted internal validation metrics
library(philentropy)       # Jensen-Shannon divergence
library(writexl)           # Excel export

# SET THIS PATH to match your replication folder (same as $path in Master.do)
mypath <- "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"

# define folders
path_rawdata          <- file.path(mypath, "Data", "Raw")
path_intermediatedata <- file.path(mypath, "Data", "intermediate")
path_cleandata        <- file.path(mypath, "Data", "clean")
path_output           <- file.path(mypath, "Output")


# ─────────────────────────────────────────────────────────────────────────────
# CLUSTERING FUNCTION
# ─────────────────────────────────────────────────────────────────────────────

run_clustering <- function(soc_level, distance_method, linkage_method,
                           desired_ks, plot_validation = FALSE,
                           Kmax = 50, csv_config = NULL,
                           xlsx_config = NULL) {

  cat(sprintf("Running: soc=%s, dist=%s, link=%s\n", soc_level, distance_method, linkage_method))

  # 1. Read data
  input_file <- file.path(path_intermediatedata,
                          sprintf("degreeoccupationsoc%s.dta", soc_level))
  df <- read_dta(input_file)

  # 2. Prepare matrix & weights
  data_matrix <- df %>%
    select(starts_with("meanocc")) %>%
    as.matrix()
  rownames(data_matrix) <- df$degfieldd

  weights <- df$totalcellsize
  names(weights) <- rownames(data_matrix)

  # 3. Compute distance
  if (distance_method == "jsd") {
    dist_obj <- philentropy::distance(data_matrix, method = "jensen-shannon",
                                      as.dist.obj = TRUE, unit = "log2")
  } else if (distance_method == "manhattan") {
    dist_obj <- dist(data_matrix, method = "manhattan")
  } else {
    stop(sprintf("Unknown distance method: %s", distance_method))
  }

  # 4. Cluster
  hc <- hclust(dist_obj, method = linkage_method, members = weights)

  # 5. Validation plots (optional)
  if (plot_validation) {
    k_values <- 2:Kmax
    ASWw_vec <- numeric(length(k_values))
    HC_vec   <- numeric(length(k_values))
    HG_vec   <- numeric(length(k_values))

    for (i in seq_along(k_values)) {
      k         <- k_values[i]
      cluster_k <- cutree(hc, k = k)
      qc    <- wcClusterQuality(diss = dist_obj, cluster = cluster_k, weights = weights)
      stats <- qc$stats
      ASWw_vec[i] <- stats["ASWw"]
      HC_vec[i]   <- stats["HC"]
      HG_vec[i]   <- stats["HG"]
    }

    validation_df <- data.frame(k = k_values, ASWw = ASWw_vec, HC = HC_vec, HG = HG_vec)

    # Build plot filename
    is_09    <- grepl("09$", soc_level)
    base_soc <- sub("09$", "", soc_level)
    soc_suffix   <- if (base_soc != "4") paste0(" SOC", base_soc) else ""
    year_suffix  <- if (is_09) " 09" else ""
    link_suffix  <- if (linkage_method != "average") paste0(" ", sub("\\..*", "", linkage_method)) else ""
    jpeg_name <- sprintf("cluster validation scores %s%s%s%s.jpeg",
                         distance_method, link_suffix, year_suffix, soc_suffix)
    jpeg_path <- file.path(path_output, jpeg_name)

    jpeg(filename = jpeg_path, width = 800, height = 1200, res = 150)
    op <- par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

    plot(validation_df$k, validation_df$ASWw, type = "b", pch = 20,
         xlab = "Number of clusters", ylab = "ASWw",
         main = "Weighted Average Silhouette Width (ASWw) - Maximisation")
    plot(validation_df$k, validation_df$HC, type = "b", pch = 20,
         xlab = "Number of clusters", ylab = "Hubert's C",
         main = "Hubert's C - Minimisation")
    plot(validation_df$k, validation_df$HG, type = "b", pch = 20,
         xlab = "Number of clusters", ylab = "Hubert's Gamma",
         main = "Hubert's Gamma - Maximisation")

    par(op)
    dev.off()
  }

  # 6. Cut tree at desired k values
  cluster_output <- data.frame(
    degfieldd = as.character(df$degfieldd),
    stringsAsFactors = FALSE
  )

  for (k in desired_ks) {
    cluster_k <- cutree(hc, k = k)
    names(cluster_k) <- rownames(data_matrix)
    col_name <- paste0("cluster", k)
    cluster_output[[col_name]] <- cluster_k[as.character(cluster_output$degfieldd)]
  }

  # 7. Save .dta
  link_name <- switch(linkage_method,
    "average" = "average_linkage",
    "ward.D2" = "ward_linkage"
  )
  output_name <- sprintf("weighted_%s_%s_soc%s.dta", link_name, distance_method, soc_level)
  save_dta <- file.path(path_intermediatedata, output_name)
  haven::write_dta(cluster_output, save_dta)
  cat(sprintf("  Saved: %s\n", output_name))

  # 8. CSV export (optional)
  if (!is.null(csv_config)) {
    label_map <- df %>%
      transmute(
        degfieldd_code  = as.numeric(degfieldd),
        degfieldd_label = as.character(haven::as_factor(degfieldd))
      ) %>%
      distinct(degfieldd_code, degfieldd_label)

    k_col <- paste0("cluster", csv_config$k)
    clusters_df <- cluster_output %>%
      mutate(degfieldd_code = as.numeric(degfieldd)) %>%
      left_join(label_map, by = "degfieldd_code") %>%
      select(degfieldd_label, all_of(k_col)) %>%
      rename(degfieldd = degfieldd_label)

    if (isTRUE(csv_config$sort)) {
      clusters_df <- clusters_df %>% arrange(.data[[k_col]])
    }

    write.csv(clusters_df, file.path(path_output, csv_config$file), row.names = FALSE)
    cat(sprintf("  Saved CSV: %s\n", csv_config$file))
  }

  # 9. XLSX export (optional)
  if (!is.null(xlsx_config)) {
    label_map <- df %>%
      transmute(
        degfieldd_code  = as.numeric(degfieldd),
        degfieldd_label = as.character(haven::as_factor(degfieldd))
      ) %>%
      distinct(degfieldd_code, degfieldd_label)

    k_col <- paste0("cluster", xlsx_config$k)
    clusters_df <- cluster_output %>%
      mutate(degfieldd_code = as.numeric(degfieldd)) %>%
      left_join(label_map, by = "degfieldd_code") %>%
      select(degfieldd_label, all_of(k_col)) %>%
      rename(degfieldd = degfieldd_label)

    if (isTRUE(xlsx_config$sort)) {
      clusters_df <- clusters_df %>% arrange(.data[[k_col]])
    }

    writexl::write_xlsx(clusters_df, file.path(path_output, xlsx_config$file))
    cat(sprintf("  Saved XLSX: %s\n", xlsx_config$file))
  }

  invisible(cluster_output)
}


# ─────────────────────────────────────────────────────────────────────────────
# RUN ALL 12 CLUSTERING CONFIGURATIONS
# ─────────────────────────────────────────────────────────────────────────────

ks_standard <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
ks_09       <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 101, 171)

configs <- list(
  # JSD + average linkage (main specs)
  list(soc = "4",   dist = "jsd",       link = "average",  ks = ks_standard,
       plot = TRUE,  Kmax = 50,
       csv = list(k = 25, file = "clusters_25_jsdsoc4.csv")),
  list(soc = "3",   dist = "jsd",       link = "average",  ks = ks_standard,
       plot = TRUE,  Kmax = 50),
  list(soc = "2",   dist = "jsd",       link = "average",  ks = ks_standard,
       plot = TRUE,  Kmax = 50),

  # Manhattan + average linkage (appendix specs)
  list(soc = "2",   dist = "manhattan", link = "average",  ks = ks_standard),
  list(soc = "3",   dist = "manhattan", link = "average",  ks = ks_standard),
  list(soc = "4",   dist = "manhattan", link = "average",  ks = ks_standard),

  # JSD + Ward linkage (appendix specs)
  list(soc = "2",   dist = "jsd",       link = "ward.D2",  ks = ks_standard,
       plot = TRUE,  Kmax = 50),
  list(soc = "3",   dist = "jsd",       link = "ward.D2",  ks = ks_standard,
       plot = TRUE,  Kmax = 50),
  list(soc = "4",   dist = "jsd",       link = "ward.D2",  ks = ks_standard,
       plot = TRUE,  Kmax = 50,
       csv  = list(k = 25, file = "clusters_25_jsdsoc4_ward.csv"),
       xlsx = list(k = 25, file = "clusters_25_jsdsoc4_ward.xlsx")),

  # JSD + average linkage, 2009 only (appendix specs)
  list(soc = "409", dist = "jsd",       link = "average",  ks = ks_09,
       plot = TRUE,  Kmax = 150,
       csv = list(k = 101, file = "clusters_101_jsdsoc4_09.csv", sort = TRUE)),
  list(soc = "309", dist = "jsd",       link = "average",  ks = ks_09,
       plot = TRUE,  Kmax = 150),
  list(soc = "209", dist = "jsd",       link = "average",  ks = ks_09,
       plot = TRUE,  Kmax = 150)
)

for (cfg in configs) {
  run_clustering(
    soc_level       = cfg$soc,
    distance_method = cfg$dist,
    linkage_method  = cfg$link,
    desired_ks      = cfg$ks,
    plot_validation = isTRUE(cfg$plot),
    Kmax            = if (!is.null(cfg$Kmax)) cfg$Kmax else 50,
    csv_config      = cfg$csv,
    xlsx_config     = cfg$xlsx
  )
}

cat("\nAll clustering configurations complete.\n")
