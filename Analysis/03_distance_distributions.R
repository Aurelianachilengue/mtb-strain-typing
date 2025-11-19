############################################################
# 03_distance_distributions.R
# Plot genetic distance distributions within and between
# lineages for a given method (e.g. MASH, SKA2, PopPUNK).
#
# Input:
#   - distance_file: tab-separated, columns:
#       Sample1  Sample2  Distance
#   - lineage_file: first column = lineage name,
#       remaining columns = genome IDs in that lineage
#
# Output:
#   - results/plots/<method>_within_between_hist.png
#   - results/plots/<method>_by_lineage_hist.png
############################################################

library(ggplot2)

#-------------------------------
# USER INPUT
#-------------------------------

method_name  <- "MASH"  # used in plot titles and filenames
# Adjust the path accordingly
distance_file <- "data/distances/mash_distances.tsv"
lineage_file  <- "data/lineage_assignment/lineages_file_tab.txt"

#-------------------------------
# Read distance data
#-------------------------------
dist_df <- read.table(
  distance_file,
  sep     = "\t",
  header  = FALSE,
  stringsAsFactors = FALSE
)
colnames(dist_df) <- c("Sample1", "Sample2", "Distance")

#-------------------------------
# Read lineage definitions
#-------------------------------
lineages_raw <- read.delim(
  lineage_file,
  header           = FALSE,
  sep              = "\t",
  fill             = TRUE,
  stringsAsFactors = FALSE
)

# Convert wide lineage table into a named vector:
# genome_id -> lineage
genome_lineage_list <- list()

for (i in seq_len(nrow(lineages_raw))) {
  lin_name <- lineages_raw[i, 1]
  genomes  <- unlist(lineages_raw[i, -1])
  genomes  <- genomes[!is.na(genomes) & genomes != ""]
  for (g in genomes) {
    genome_lineage_list[[g]] <- lin_name
  }
}

genome_lineage <- unlist(genome_lineage_list)

#-------------------------------
# Annotate distance pairs
#-------------------------------
dist_df$LS1 <- genome_lineage[dist_df$Sample1]
dist_df$LS2 <- genome_lineage[dist_df$Sample2]

# TRUE if within same lineage, FALSE if between
dist_df$within <- dist_df$LS1 == dist_df$LS2

# Factor: lineage name for within-lineage pairs,
#         "between" for inter-lineage pairs
dist_df$lin_group <- ifelse(
  dist_df$within & !is.na(dist_df$within),
  dist_df$LS1,
  "between"
)

#-------------------------------
# Custom colours
#-------------------------------
custom_colors <- c(
  "L1" = "#a6cee3",
  "L2" = "#1f78b4",
  "L3" = "#b2df8a",
  "L4" = "#33a02c",
  "L5" = "#fb9a99",
  "L6" = "#e31a1c",
  "L7" = "#fdbf6f",
  "L8" = "#cab2d6",
  "L9" = "#6a3d9a",
  "La1" = "#ffff99",
  "La2" = "#b15928",
  "La3" = "#8dd3c7",
  "microti"    = "#FF69B4",
  "pinnipedii" = "#bc80bd",
  "between"    = "#ff8c00"   # orange for between-lineage
)

# Create output folder if needed
if (!dir.exists("results/plots")) dir.create("results/plots", recursive = TRUE)

#-------------------------------
# Histogram 1: Within vs Between
#-------------------------------
p_within_between <- ggplot(dist_df, aes(x = Distance, fill = within)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  scale_fill_manual(
    values = c("TRUE" = "#1f78b4", "FALSE" = "#ff8c00"),
    labels = c("Within", "Between"),
    name   = "Comparison"
  ) +
  ggtitle(paste(method_name, "- Genetic Distances (Within vs Between)")) +
  xlab("Genetic distance") +
  ylab("Count") +
  theme_minimal()

ggsave(
  filename = paste0("results/plots/", method_name, "_within_between_hist.png"),
  plot     = p_within_between,
  width    = 8,
  height   = 5,
  dpi      = 300
)

#-------------------------------
# Histogram 2: By lineage
#-------------------------------
p_by_lineage <- ggplot(dist_df, aes(x = Distance, fill = lin_group)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  scale_fill_manual(values = custom_colors, name = "Lineage / Between") +
  ggtitle(paste(method_name, "- Genetic Distances by Lineage")) +
  xlab("Genetic distance") +
  ylab("Count") +
  theme_minimal()

ggsave(
  filename = paste0("results/plots/", method_name, "_by_lineage_hist.png"),
  plot     = p_by_lineage,
  width    = 8,
  height   = 5,
  dpi      = 300
)
