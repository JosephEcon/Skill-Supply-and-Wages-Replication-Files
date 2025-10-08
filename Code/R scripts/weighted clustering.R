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

# set base path
mypath <- "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"

# define folders
path_rawdata          <- file.path(mypath, "Data","Raw")
path_intermediatedata <- file.path(mypath, "Data", "intermediate")
path_cleandata        <- file.path(mypath, "Data", "clean")
path_output           <- file.path(mypath, "output")



#Jensen shannon divergence soc4



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc4.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 50
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)

desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc4.dta")
haven::write_dta(cluster_output, save_dta)

#Saving clusters to excel

label_map <- df %>%
  transmute(
    degfieldd_code  = as.numeric(degfieldd),                    # numeric code
    degfieldd_label = as.character(haven::as_factor(degfieldd)) # text label
  ) %>%
  distinct(degfieldd_code, degfieldd_label)

# join labels onto cluster_output (make sure cluster_output$degfieldd is numeric or coerced)
clusters_k25_df <- cluster_output %>%
  mutate(degfieldd_code = as.numeric(degfieldd)) %>%
  left_join(label_map, by = "degfieldd_code") %>%
  select(degfieldd_label, cluster25) %>%
  rename(degfieldd = degfieldd_label)   # replace code column with label text


write.csv(clusters_k25_df, file.path(path_output, "clusters_25_jsdsoc4.csv"), row.names = FALSE)

#Jensen shannon divergence soc3




df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc3.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 50
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd SOC3.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc3.dta")
haven::write_dta(cluster_output, save_dta)





#Jensen shannon divergence soc2



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc2.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 50
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd SOC2.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc2.dta")
haven::write_dta(cluster_output, save_dta)




#Manhattan average linkage soc2



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc2.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance <- dist(data_matrix, method="manhattan")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (Average linkage), passing in observation weights
hc <- hclust(
  distance,
  method  = "average",
  members = weights
)



# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)



desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_manhattan_soc2.dta")
haven::write_dta(cluster_output, save_dta)


#Manhattan average linkage soc3



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc3.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Manhattan distance matrix
distance <- dist(data_matrix, method="manhattan")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (Average linkage), passing in observation weights
hc <- hclust(
  distance,
  method  = "average",
  members = weights
)



# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_manhattan_soc3.dta")
haven::write_dta(cluster_output, save_dta)

#Manhattan average linkage soc4



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc4.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Manhattan distance matrix
distance <- dist(data_matrix, method="manhattan")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (Average linkage), passing in observation weights
hc <- hclust(
  distance,
  method  = "average",
  members = weights
)



# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_manhattan_soc4.dta")
haven::write_dta(cluster_output, save_dta)


#Jensen shannon divergence Ward Linkage soc2



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc2.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "ward.D2",
  members = weights
)




# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_ward_linkage_jsd_soc2.dta")
haven::write_dta(cluster_output, save_dta)


#Jensen shannon divergence Ward Linkage soc3



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc3.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "ward.D2",
  members = weights
)




# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_ward_linkage_jsd_soc3.dta")
haven::write_dta(cluster_output, save_dta)

#Jensen shannon divergence Ward Linkage soc4



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc4.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "ward.D2",
  members = weights
)




# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS .dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 175)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata .dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_ward_linkage_jsd_soc4.dta")
haven::write_dta(cluster_output, save_dta)


# With 09 occupation patters only
#Jensen shannon divergence 09 soc4



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc409.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 150
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd 09.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS 09.dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 101, 171)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata 09.dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc409.dta")
haven::write_dta(cluster_output, save_dta)

#Saving clusters to excel

label_map <- df %>%
  transmute(
    degfieldd_code  = as.numeric(degfieldd),                    # numeric code
    degfieldd_label = as.character(haven::as_factor(degfieldd)) # text label
  ) %>%
  distinct(degfieldd_code, degfieldd_label)

# join labels onto cluster_output (make sure cluster_output$degfieldd is numeric or coerced)
clusters_k101_df <- cluster_output %>%
  mutate(degfieldd_code = as.numeric(degfieldd)) %>%
  left_join(label_map, by = "degfieldd_code") %>%
  select(degfieldd_label, cluster101) %>%
  rename(degfieldd = degfieldd_label)%>%
  arrange(cluster101)   # <-- sort by cluster number ascending
# replace code column with label text


write.csv(clusters_k101_df, file.path(path_output, "clusters_101_jsdsoc4_09.csv"), row.names = FALSE)

#Jensen shannon divergence 09 soc3



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc309.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 150
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd 09 SOC3.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS 09.dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 101, 171)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata 09.dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc309.dta")
haven::write_dta(cluster_output, save_dta)





#Jensen shannon divergence 09 soc2



df <- read_dta(file.path(path_intermediatedata, "degreeoccupationsoc209.dta"))


# ─────────────────────────────────────────────────────────────────────────────
# PREPARE MATRIX & WEIGHTS
# ─────────────────────────────────────────────────────────────────────────────
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()
rownames(data_matrix) <- df$degfieldd

weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)   # ensure names match rows in data_matrix


# ─────────────────────────────────────────────────────────────────────────────
# 1) DISTANCE & CLUSTERING
# ─────────────────────────────────────────────────────────────────────────────
# Compute Jensen-Shannon distance matrix
distance_jsd <- philentropy::distance(data_matrix, method = "jensen-shannon", as.dist.obj = TRUE, unit = "log2")
names(weights) <- rownames(data_matrix)   

# Perform weighted UPGMA (average linkage), passing in observation weights
hc <- hclust(
  distance_jsd,
  method  = "average",
  members = weights
)



##Plotting cluster validation metrics
Kmax     <- 150
k_values <- 2:Kmax

ASWw_vec <- numeric(length(k_values))
HC_vec   <- numeric(length(k_values))
HG_vec   <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k         <- k_values[i]
  cluster_k <- cutree(hc, k = k)
  
  qc    <- wcClusterQuality(diss    = distance_jsd,
                            cluster = cluster_k,
                            weights = weights)
  stats <- qc$stats
  
  ASWw_vec[i] <- stats["ASWw"]
  HC_vec[i]   <- stats["HC"]
  HG_vec[i]   <- stats["HG"]
}

validation_df <- data.frame(
  k    = k_values,
  ASWw = ASWw_vec,
  HC   = HC_vec,
  HG   = HG_vec
)
# Plotting: three‐panel figure (stacked vertically)
# 1) Define the JPEG output path
jpeg_path <- file.path(path_output, "cluster validation scores jsd 09 SOC2.jpeg")

# 2) Open a JPEG device
#    You can adjust width, height, and resolution (res) as needed.
jpeg(
  filename = jpeg_path,
  width    = 800,    # in pixels
  height   = 1200,   # in pixels
  res      = 150     # dots per inch; increase if you need higher quality
)

# Set up a 3‐row plotting area
op <- par(mfrow = c(3, 1),       # 3 rows, 1 column
          mar = c(4, 4, 2, 1))    # margins: bottom, left, top, right

# 3) Plot ASWw vs k
plot(
  validation_df$k, validation_df$ASWw,
  type = "b",
  xlab = "Number of clusters",
  ylab = "ASWw",
  main = "Weighted Average Silhouette Width (ASWw) - Maximisation",
  pch  = 20
)

# 2) Plot HC vs k
plot(
  validation_df$k, validation_df$HC,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's C",
  main = "Hubert's C - Minimisation",
  pch  = 20
)

# 3) Plot HG vs k
plot(
  validation_df$k, validation_df$HG,
  type = "b",
  xlab = "Number of clusters",
  ylab = "Hubert's Gamma",
  main = "Hubert's Gamma - Maximisation",
  pch  = 20
)

# Restore default plotting parameters
par(op)
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 2) EXTRACT CLUSTER ASSIGNMENTS AT k = 20, 30, 40, 50 AND SO ON SAVE AS 09.dta
# ─────────────────────────────────────────────────────────────────────────────

# Define the specific k values for which we want assignment columns
cluster_output <- data.frame(
  degfieldd = as.character(df$degfieldd),
  stringsAsFactors = FALSE
)


desired_ks <- c(20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 101, 171)
for (k in desired_ks) {
  # cut tree
  cluster_k <- cutree(hc, k = k)
  
  # name the vector so we can safely match
  names(cluster_k) <- rownames(data_matrix)
  
  # align by degfieldd
  col_name <- paste0("cluster", k)
  cluster_output[[col_name]] <- cluster_k[ as.character(cluster_output$degfieldd) ]
}


# Save to Stata 09.dta file in the output folder
save_dta <- file.path(path_intermediatedata, "weighted_average_linkage_jsd_soc209.dta")
haven::write_dta(cluster_output, save_dta)


