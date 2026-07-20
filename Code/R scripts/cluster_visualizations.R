# ==============================================================================
#  Script:  cluster_visualizations.R
#  Purpose: Produce visualizations of the k=25 JSD + average linkage
#           clustering solution (SOC4 occupations):
#           1. Dendrogram with color-coded clusters
#           2. Heatmap + dendrogram (occupation shares)
#           3. Network / force-directed graphs (1st, 2nd, 5th percentile)
#           4. PCA scatter plot (PC1 vs PC2, colored by cluster)
#  Inputs:  Data/intermediate/degreeoccupationsoc4.dta
#  Outputs: Output/dendrogram_k25.png
#           Output/heatmap_k25.png
#           Output/network_k25_p01.png
#           Output/network_k25_p02.png
#           Output/network_k25_p05.png
#           Output/network_k25_p02_single.png
#           Output/network_k25_p02_complete.png
#           Output/network_k25_p02_ward.png
#           Output/pca_clusters_k25.png
#           Output/pca_clusters_k25_3d.html
# ==============================================================================

# --- ensure CRAN mirror and user library ---
options(repos = c(CRAN = "https://cloud.r-project.org"))
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && !dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

# --- install packages if needed ---
required_pkgs <- c("haven", "dplyr", "philentropy", "fastcluster",
                    "dendextend", "ggplot2", "ggrepel", "pheatmap",
                    "igraph", "ggraph", "RColorBrewer", "grid", "gridExtra",
                    "plotly", "htmlwidgets")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = user_lib)
  }
}

library(haven)
library(dplyr)
library(philentropy)
library(fastcluster)
library(dendextend)
library(ggplot2)
library(pheatmap)
library(igraph)
library(ggraph)
library(RColorBrewer)
library(grid)
library(gridExtra)


# ==============================================================================
# PATHS
# ==============================================================================

mypath <- "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
path_intermediate <- file.path(mypath, "Data", "intermediate")
path_output       <- file.path(mypath, "Output")


# ==============================================================================
# LOAD DATA & REPRODUCE CLUSTERING
# ==============================================================================

# Read the occupation-share matrix
df <- read_dta(file.path(path_intermediate, "degreeoccupationsoc4.dta"))

# Prepare matrix and weights (same as weighted clustering.R)
data_matrix <- df %>%
  select(starts_with("meanocc")) %>%
  as.matrix()

# Get degree field labels (make unique to handle duplicates)
label_map <- df %>%
  transmute(
    degfieldd_code  = as.numeric(degfieldd),
    degfieldd_label = as.character(haven::as_factor(degfieldd))
  ) %>%
  distinct(degfieldd_code, degfieldd_label)

labels_vec <- make.unique(label_map$degfieldd_label, sep = " ")
rownames(data_matrix) <- labels_vec
weights <- df$totalcellsize
names(weights) <- rownames(data_matrix)

# Compute JSD distance
dist_obj <- philentropy::distance(data_matrix, method = "jensen-shannon",
                                   as.dist.obj = TRUE, unit = "log2")

# Hierarchical clustering (average linkage, weighted)
hc <- fastcluster::hclust(dist_obj, method = "average", members = weights)
hc$labels <- rownames(data_matrix)

# Cut at k=25
clusters_25 <- cutree(hc, k = 25)

cat("Clustering reproduced. Number of majors:", length(clusters_25), "\n")
cat("Cluster sizes:\n")
print(table(clusters_25))


# ==============================================================================
# COLOR PALETTE (25 distinct colors for clusters)
# ==============================================================================

# Use a combination of qualitative palettes for 25 distinct colors
palette_25 <- c(
  brewer.pal(12, "Set3"),
  brewer.pal(8, "Dark2"),
  brewer.pal(9, "Set1")
)[1:25]


# ==============================================================================
# VISUALIZATION 1: DENDROGRAM
# ==============================================================================

cat("\nCreating dendrogram...\n")

dend <- as.dendrogram(hc)

# Color branches by k=25 cluster assignment
dend <- color_branches(dend, k = 25, col = palette_25)

# Shrink labels for readability
dend <- set(dend, "labels_cex", 0.25)

# Color labels by cluster
cluster_colors <- palette_25[clusters_25[labels(dend)]]
dend <- set(dend, "labels_col", cluster_colors)

png(file.path(path_output, "dendrogram_k25.png"),
    width = 3000, height = 2000, res = 200, bg = "white")

par(mar = c(12, 4, 3, 1))
plot(dend,
     main = "Hierarchical Clustering of Undergraduate Majors (k = 25)",
     ylab = "Jensen-Shannon Divergence (Average Linkage)",
     leaflab = "perpendicular")

# Add a horizontal line at the cut height
abline(h = heights_per_k.dendrogram(dend)["25"], lty = 2, col = "gray50")

dev.off()

cat("  Saved: dendrogram_k25.png\n")


# ==============================================================================
# VISUALIZATION 2: HEATMAP + DENDROGRAM
# ==============================================================================

cat("Creating heatmap...\n")

# Select the top occupations by overall prevalence (mean share across majors)
occ_means <- colMeans(data_matrix)
top_n_occ <- 40
top_occ <- names(sort(occ_means, decreasing = TRUE))[1:top_n_occ]

# Subset the data matrix to top occupations
heatmap_matrix <- data_matrix[, top_occ]

# Clean up column names: remove "meanocc_" prefix
colnames(heatmap_matrix) <- gsub("^meanocc_", "SOC ", colnames(heatmap_matrix))

# Row annotation: cluster assignment
row_annotation <- data.frame(
  Cluster = factor(clusters_25[rownames(heatmap_matrix)]),
  row.names = rownames(heatmap_matrix)
)

# Cluster colors for annotation
ann_colors <- list(
  Cluster = setNames(palette_25, as.character(1:25))
)

png(file.path(path_output, "heatmap_k25.png"),
    width = 3200, height = 4000, res = 200, bg = "white")

pheatmap(heatmap_matrix,
         cluster_rows = hc,
         cluster_cols = TRUE,
         clustering_distance_cols = "euclidean",
         clustering_method = "average",
         annotation_row = row_annotation,
         annotation_colors = ann_colors,
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 4,
         fontsize_col = 7,
         fontsize = 8,
         color = colorRampPalette(c("white", "steelblue1", "navy"))(100),
         main = "Occupation Shares by Major (Top 40 Occupations, k = 25 Clusters)",
         border_color = NA,
         treeheight_row = 100,
         treeheight_col = 50)

dev.off()

cat("  Saved: heatmap_k25.png\n")


# ==============================================================================
# VISUALIZATION 3: NETWORK / FORCE-DIRECTED GRAPHS (1st, 2nd, 5th percentile)
# ==============================================================================

cat("\nCreating network graphs...\n")

# Convert distance object to matrix
dist_matrix <- as.matrix(dist_obj)
rownames(dist_matrix) <- rownames(data_matrix)
colnames(dist_matrix) <- rownames(data_matrix)

all_dists <- dist_matrix[upper.tri(dist_matrix)]

# Loop over three percentile thresholds
percentiles <- c(0.01, 0.02, 0.05)
pct_labels  <- c("1st", "2nd", "5th")
pct_tags    <- c("p01", "p02", "p05")

for (i in seq_along(percentiles)) {

  pct   <- percentiles[i]
  label <- pct_labels[i]
  tag   <- pct_tags[i]

  threshold <- quantile(all_dists, probs = pct)
  cat(sprintf("\n  --- %s percentile (JSD < %.4f) ---\n", label, threshold))

  # Build adjacency: 1 if distance < threshold, 0 otherwise
  adj_matrix <- (dist_matrix < threshold) * 1
  diag(adj_matrix) <- 0

  # Create igraph object
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = NULL)

  # Add node attributes
  V(g)$cluster <- as.character(clusters_25[V(g)$name])
  V(g)$color   <- palette_25[clusters_25[V(g)$name]]

  # Node size proportional to totalcellsize
  V(g)$size <- scales::rescale(weights[V(g)$name], to = c(2, 8))

  # Remove isolates (nodes with no edges at this threshold)
  isolates <- which(degree(g) == 0)
  if (length(isolates) > 0) {
    isolated_names <- V(g)$name[isolates]
    cat(sprintf("  Removing %d isolated nodes\n", length(isolates)))
    g <- delete_vertices(g, isolates)
  }

  cat(sprintf("  Network: %d nodes, %d edges\n", vcount(g), ecount(g)))

  # Shorten labels for display
  short_labels <- V(g)$name
  short_labels[nchar(short_labels) > 30] <-
    paste0(substr(short_labels[nchar(short_labels) > 30], 1, 27), "...")

  p_network <- ggraph(g, layout = "fr") +
    geom_edge_link(alpha = 0.2, color = "gray50") +
    geom_node_point(aes(color = cluster, size = size), alpha = 0.9) +
    geom_node_text(aes(label = short_labels), size = 2.5, repel = TRUE,
                   max.overlaps = 25, segment.alpha = 0.3,
                   fontface = "bold", color = "gray20") +
    scale_color_manual(values = setNames(palette_25, as.character(1:25)),
                       name = "Cluster") +
    scale_size_identity() +
    labs(title = "Network of Undergraduate Majors by Occupational Similarity",
         subtitle = sprintf("Edges connect majors with JSD < %.3f (%s percentile)",
                            threshold, label)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(ncol = 1, override.aes = list(size = 4)))

  outfile <- file.path(path_output, sprintf("network_k25_%s.png", tag))
  ggsave(outfile, p_network, width = 16, height = 12, dpi = 200, bg = "white")
  cat(sprintf("  Saved: network_k25_%s.png\n", tag))
}


# ==============================================================================
# VISUALIZATION 3b: NETWORK GRAPHS BY LINKAGE METHOD (2nd percentile)
# ==============================================================================

cat("\nCreating network graphs for alternative linkage methods...\n")

# Use the 2nd percentile threshold (same edges for all methods)
threshold_2 <- quantile(all_dists, probs = 0.02)

adj_matrix_2 <- (dist_matrix < threshold_2) * 1
diag(adj_matrix_2) <- 0

# Linkage methods to compare
linkage_methods <- c("single", "complete", "ward.D2")
linkage_labels  <- c("Single Linkage", "Complete Linkage", "Ward's Method")
linkage_tags    <- c("single", "complete", "ward")

for (j in seq_along(linkage_methods)) {

  method_name  <- linkage_methods[j]
  method_label <- linkage_labels[j]
  method_tag   <- linkage_tags[j]

  cat(sprintf("\n  --- %s ---\n", method_label))

  # Cluster with this linkage method
  hc_alt <- fastcluster::hclust(dist_obj, method = method_name, members = weights)
  hc_alt$labels <- rownames(data_matrix)
  clusters_alt <- cutree(hc_alt, k = 25)

  cat("  Cluster sizes:\n")
  print(table(clusters_alt))

  # Build graph (same edges as 2nd percentile average linkage)
  g_alt <- graph_from_adjacency_matrix(adj_matrix_2, mode = "undirected", weighted = NULL)

  # Color by this method's clusters
  V(g_alt)$cluster <- as.character(clusters_alt[V(g_alt)$name])
  V(g_alt)$color   <- palette_25[clusters_alt[V(g_alt)$name]]
  V(g_alt)$size    <- scales::rescale(weights[V(g_alt)$name], to = c(2, 8))

  # Remove isolates
  isolates_alt <- which(degree(g_alt) == 0)
  if (length(isolates_alt) > 0) {
    cat(sprintf("  Removing %d isolated nodes\n", length(isolates_alt)))
    g_alt <- delete_vertices(g_alt, isolates_alt)
  }

  cat(sprintf("  Network: %d nodes, %d edges\n", vcount(g_alt), ecount(g_alt)))

  # Shorten labels
  short_labels_alt <- V(g_alt)$name
  short_labels_alt[nchar(short_labels_alt) > 30] <-
    paste0(substr(short_labels_alt[nchar(short_labels_alt) > 30], 1, 27), "...")

  p_alt <- ggraph(g_alt, layout = "fr") +
    geom_edge_link(alpha = 0.2, color = "gray50") +
    geom_node_point(aes(color = cluster, size = size), alpha = 0.9) +
    geom_node_text(aes(label = short_labels_alt), size = 2.5, repel = TRUE,
                   max.overlaps = 25, segment.alpha = 0.3,
                   fontface = "bold", color = "gray20") +
    scale_color_manual(values = setNames(palette_25, as.character(1:25)),
                       name = "Cluster") +
    scale_size_identity() +
    labs(title = sprintf("Network of Undergraduate Majors (%s)", method_label),
         subtitle = sprintf("Edges: JSD < %.3f (2nd percentile). Clusters: k = 25, %s on JSD.",
                            threshold_2, method_label)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(ncol = 1, override.aes = list(size = 4)))

  outfile_alt <- file.path(path_output, sprintf("network_k25_p02_%s.png", method_tag))
  ggsave(outfile_alt, p_alt, width = 16, height = 12, dpi = 200, bg = "white")
  cat(sprintf("  Saved: network_k25_p02_%s.png\n", method_tag))
}


# ==============================================================================
# VISUALIZATION 4: PCA SCATTER PLOT (PC1 vs PC2, colored by cluster)
# ==============================================================================

cat("\nCreating PCA scatter plot...\n")

# Run PCA on the occupation-share matrix
pca_result <- prcomp(data_matrix, center = TRUE, scale. = FALSE)

# Variance explained
var_explained <- summary(pca_result)$importance[2, 1:3] * 100  # % for PC1–PC3
cat(sprintf("  PC1 explains %.1f%% of variance\n", var_explained[1]))
cat(sprintf("  PC2 explains %.1f%% of variance\n", var_explained[2]))
cat(sprintf("  PC3 explains %.1f%% of variance\n", var_explained[3]))

# Build data frame for plotting
pca_df <- data.frame(
  PC1     = pca_result$x[, 1],
  PC2     = pca_result$x[, 2],
  PC3     = pca_result$x[, 3],
  major   = rownames(data_matrix),
  cluster = factor(clusters_25[rownames(data_matrix)]),
  stringsAsFactors = FALSE
)

# Shorten labels for display
pca_df$short_label <- pca_df$major
long <- nchar(pca_df$short_label) > 30
pca_df$short_label[long] <- paste0(substr(pca_df$short_label[long], 1, 27), "...")

# Node size proportional to totalcellsize
pca_df$weight <- weights[pca_df$major]

p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster, size = weight)) +
  geom_point(alpha = 0.85) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  ggrepel::geom_text_repel(
    aes(label = short_label),
    size = 2.2, max.overlaps = 20, segment.alpha = 0.3,
    fontface = "bold", color = "gray20"
  ) +
  scale_color_manual(values = setNames(palette_25, as.character(1:25)),
                     name = "Cluster") +
  labs(
    title = "PCA of Undergraduate Majors by Occupation Shares",
    x = sprintf("PC1 (%.1f%% of variance)", var_explained[1]),
    y = sprintf("PC2 (%.1f%% of variance)", var_explained[2])
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    axis.title    = element_text(size = 11, face = "bold"),
    axis.text     = element_text(size = 9),
    legend.title  = element_text(size = 10, face = "bold"),
    legend.text   = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 4)))

ggsave(file.path(path_output, "pca_clusters_k25.png"), p_pca,
       width = 16, height = 12, dpi = 200, bg = "white")

cat("  Saved: pca_clusters_k25.png\n")


# ==============================================================================
# VISUALIZATION 5: 3D PCA SCATTER PLOT (PC1 x PC2 x PC3, interactive)
# ==============================================================================

cat("\nCreating 3D PCA scatter plot...\n")

# Assign a color to each major based on its cluster
pca_df$color <- palette_25[as.integer(as.character(pca_df$cluster))]

# Rescale weights for 3D marker sizes
pca_df$marker_size <- scales::rescale(pca_df$weight, to = c(3, 12))

p3d <- plotly::plot_ly(
  data = pca_df,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~cluster,
  colors = setNames(palette_25, as.character(1:25)),
  size = ~marker_size,
  sizes = c(3, 12),
  text = ~major,
  hoverinfo = "text",
  type = "scatter3d",
  mode = "markers",
  marker = list(opacity = 0.9)
) %>%
  plotly::layout(
    title = list(
      text = "PCA of Undergraduate Majors by Occupation Shares (3D)",
      font = list(size = 16, color = "black")
    ),
    scene = list(
      xaxis = list(title = sprintf("PC1 (%.1f%%)", var_explained[1])),
      yaxis = list(title = sprintf("PC2 (%.1f%%)", var_explained[2])),
      zaxis = list(title = sprintf("PC3 (%.1f%%)", var_explained[3])),
      bgcolor = "white"
    ),
    paper_bgcolor = "white",
    legend = list(title = list(text = "Cluster"))
  )

htmlwidgets::saveWidget(p3d,
  file = file.path(path_output, "pca_clusters_k25_3d.html"),
  selfcontained = FALSE)

cat("  Saved: pca_clusters_k25_3d.html\n")


cat("\nAll cluster visualizations complete.\n")
