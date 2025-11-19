############################################################
# 02_membership_ratio_plots.R
#
# Plot lineage cluster membership ratios for four methods:
# DNAdiff, PopPUNK, MASH and SKA2.
#
# Input (one Excel file per method):
#   - Column 1: Lineage name (e.g. L1, L2, L3, La1, microti)
#   - Column 2: n_in_cluster  (genomes grouped in that lineage)
#   - Column 3: n_total       (total genomes expected in that lineage)
#   - Column 4: Ratio         (n_in_cluster / n_total)
#
# Output:
#   - PNG figure: Lineage_ratio_methods_coloured_by_method.png
#
# Dependencies: readxl, dplyr, ggplot2
############################################################

library(readxl)
library(dplyr)
library(ggplot2)

#-----------------------------------------------------------
# Helper function to load one method’s results
#-----------------------------------------------------------
load_method <- function(path, method_name) {
  df <- read_excel(path)

  # Rename columns explicitly
  colnames(df)[1] <- "Lineages"
  colnames(df)[2] <- "n_in_cluster"
  colnames(df)[3] <- "n_total"
  colnames(df)[4] <- "Ratio"

  # Recompute ratio to be sure it matches the definition
  df <- df %>%
    mutate(
      Method = method_name,
      Ratio  = n_in_cluster / n_total
    ) %>%
    select(Lineages, Method, Ratio)

  return(df)
}

#-----------------------------------------------------------
# Load each method file
#  (update paths to match your repo layout)
#-----------------------------------------------------------

dnadiff <- load_method(
  "data/lineage_assignment/Lineage_DNAdiff.xlsx",
  "DNAdiff"
)

poppunk <- load_method(
  "data/lineage_assignment/Lineage_PopPUNK.xlsx",
  "PopPUNK"
)

mash <- load_method(
  "data/lineage_assignment/Lineage_MASH.xlsx",
  "MASH"
)

ska2 <- load_method(
  "data/lineage_assignment/Lineage_SKA2.xlsx",
  "SKA2"
)

# Merge all methods into one data.frame
all_methods <- bind_rows(dnadiff, poppunk, mash, ska2)

#-----------------------------------------------------------
# Factor order for methods and lineages
#-----------------------------------------------------------

# Order of methods in legend
all_methods$Method <- factor(
  all_methods$Method,
  levels = c("DNAdiff", "PopPUNK", "MASH", "SKA2")
)

# Order of lineages on x-axis
all_methods$Lineages <- factor(
  all_methods$Lineages,
  levels = c("L1", "L2", "L3", "L4", "L5",
             "L6", "L7", "La1", "La2", "microti")
)

#-----------------------------------------------------------
# Plot
#-----------------------------------------------------------

p <- ggplot(
  all_methods,
  aes(x = Lineages, y = Ratio,
      colour = Method, shape = Method)
) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.6)
  ) +
  # Shapes per method
  scale_shape_manual(values = c(
    "DNAdiff" = 8,   # star
    "PopPUNK" = 17,  # triangle
    "MASH"    = 15,  # square
    "SKA2"    = 16   # circle
  )) +
  # Colours per method
  scale_colour_manual(values = c(
    "DNAdiff" = "#6A3D9A",  # purple
    "PopPUNK" = "#FF1493",  # strong pink
    "MASH"    = "#FBB41A",  # yellow / amber
    "SKA2"    = "#0B84A5"   # teal/blue
  )) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_hline(
    yintercept = c(0, 0.5, 1),
    linetype = "dashed",
    color = "grey70"
  ) +
  labs(
    title  = "Cluster Membership Ratio Across Lineages and Methods",
    x      = "Lineages",
    y      = "Ratio",
    colour = "Method",
    shape  = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
    legend.text  = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold")
  )

# Print to screen (if running interactively)
print(p)

# Save to file
ggsave(
  filename = "results/plots/Lineage_ratio_methods_coloured_by_method.png",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300
)
