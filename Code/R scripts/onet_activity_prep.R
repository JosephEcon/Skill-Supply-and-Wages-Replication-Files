# ==============================================================================
#  onet_activity_prep.R
#  Purpose: Parse the O*NET database text files into occupation x work-activity
#           BINARY "requires" matrices at three granularities:
#             GWA  - Generalized Work Activities (41), rated; requires = Importance >= 3
#             IWA  - Intermediate Work Activities (~330), task-derived (binary)
#             DWA  - Detailed Work Activities (~2000), task-derived (binary)
#           Keyed by a 6-character SOC code ("soc6") compatible with the IPUMS
#           ACS occsoc variable (str6, no punctuation), built by stripping the
#           "-" and ".xx" detail suffix from the O*NET-SOC code and averaging /
#           unioning across the detailed O*NET-SOC codes within each soc6.
#
#  Inputs : Data/Raw/onet/db_23_0_text/{Work Activities, Tasks to DWAs,
#           DWA Reference, IWA Reference}.txt   (O*NET 23.0, SOC-2010 based)
#  Outputs: Data/intermediate/onet_requires_{gwa,iwa,dwa}.dta  (soc6, activity_id, requires)
#           Data/intermediate/onet_labels_{gwa,iwa,dwa}.dta    (activity_id, title)
#
#  Note   : Saves only the requires==1 "edges" (sparse). task clustering.R builds
#           the full soc6 x activity {0,1} matrix from these, filling 0 elsewhere.
# ==============================================================================

# --- CRAN mirror & user library (mirrors weighted clustering.R) ---
options(repos = c(CRAN = "https://cloud.r-project.org"))
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && !dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

for (pkg in c("dplyr", "haven")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
suppressMessages({ library(dplyr); library(haven) })

# --- paths ---
mypath <- "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
path_rawdata          <- file.path(mypath, "Data", "Raw")
path_intermediatedata <- file.path(mypath, "Data", "intermediate")
onet_dir              <- file.path(path_rawdata, "onet", "db_23_0_text")

IMPORTANCE_THRESHOLD <- 3.0   # GWA "requires" cutoff on the 1-5 Importance scale

# --- helpers ---
read_onet <- function(f) {
  read.delim(file.path(onet_dir, f), sep = "\t", quote = "",
             stringsAsFactors = FALSE, check.names = FALSE,
             colClasses = "character", na.strings = c("", "n/a"))
}

# O*NET-SOC "11-1011.03" -> soc6 "111011" (IPUMS occsoc-compatible)
to_soc6 <- function(x) {
  x <- gsub("-", "", x, fixed = TRUE)   # "111011.03"
  x <- sub("\\..*$", "", x)             # "111011"
  substr(x, 1, 6)
}

cat("Reading O*NET text files from:", onet_dir, "\n")

# ------------------------------------------------------------------------------
# 1. GWA  (Generalized Work Activities): rated, requires = Importance >= threshold
# ------------------------------------------------------------------------------
wa <- read_onet("Work Activities.txt")
names(wa)[match(c("O*NET-SOC Code","Element ID","Element Name","Scale ID","Data Value"),
                names(wa))] <- c("onetsoc","activity_id","title","scale","value")

gwa <- wa %>%
  filter(scale == "IM") %>%
  mutate(soc6 = to_soc6(onetsoc), value = as.numeric(value)) %>%
  group_by(soc6, activity_id) %>%
  summarise(mean_importance = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(requires = as.integer(mean_importance >= IMPORTANCE_THRESHOLD)) %>%
  filter(requires == 1L) %>%
  select(soc6, activity_id, requires)

gwa_labels <- wa %>% distinct(activity_id, title)

# ------------------------------------------------------------------------------
# 2. DWA (Detailed Work Activities): occupation has a DWA if any of its tasks
#    maps to that DWA (Tasks to DWAs). Binary; soc6 = union across detailed codes.
# ------------------------------------------------------------------------------
t2d <- read_onet("Tasks to DWAs.txt")
names(t2d)[match(c("O*NET-SOC Code","DWA ID"), names(t2d))] <- c("onetsoc","activity_id")
t2d <- t2d %>% mutate(soc6 = to_soc6(onetsoc))

dwa <- t2d %>% distinct(soc6, activity_id) %>% mutate(requires = 1L)

dwa_ref <- read_onet("DWA Reference.txt")
names(dwa_ref)[match(c("IWA ID","DWA ID","DWA Title"), names(dwa_ref))] <- c("iwa_id","activity_id","title")
dwa_labels <- dwa_ref %>% distinct(activity_id, title)

# ------------------------------------------------------------------------------
# 3. IWA (Intermediate Work Activities): roll DWA up to parent IWA via DWA
#    Reference. Occupation requires IWA if it requires any child DWA. Binary.
# ------------------------------------------------------------------------------
d2i <- dwa_ref %>% distinct(dwa_id = activity_id, iwa_id)

iwa <- t2d %>%
  select(soc6, dwa_id = activity_id) %>%
  left_join(d2i, by = "dwa_id") %>%
  filter(!is.na(iwa_id)) %>%
  distinct(soc6, activity_id = iwa_id) %>%
  mutate(requires = 1L)

iwa_ref <- read_onet("IWA Reference.txt")
names(iwa_ref)[match(c("IWA ID","IWA Title"), names(iwa_ref))] <- c("activity_id","title")
iwa_labels <- iwa_ref %>% distinct(activity_id, title)

# ------------------------------------------------------------------------------
# Save + report
# ------------------------------------------------------------------------------
save_level <- function(edges, labels, level) {
  write_dta(edges,  file.path(path_intermediatedata, sprintf("onet_requires_%s.dta", level)))
  write_dta(labels, file.path(path_intermediatedata, sprintf("onet_labels_%s.dta",   level)))
  cat(sprintf("  %-3s: %5d occupations(soc6) x %4d activities, %7d requires-edges (density %.3f)\n",
              toupper(level),
              length(unique(edges$soc6)),
              length(unique(edges$activity_id)),
              nrow(edges),
              nrow(edges) / (length(unique(edges$soc6)) * length(unique(edges$activity_id)))))
}

cat("\nO*NET requires matrices (saved to intermediate):\n")
save_level(gwa, gwa_labels, "gwa")
save_level(iwa, iwa_labels, "iwa")
save_level(dwa, dwa_labels, "dwa")

cat("\nGWA importance threshold for 'requires': >=", IMPORTANCE_THRESHOLD, "\n")
cat("onet_activity_prep.R complete.\n")
