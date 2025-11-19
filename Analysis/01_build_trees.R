# ------------------------------------------------------------
# Build rooted neighbor-joining trees from a symmetric
# genetic distance matrix.
#
# Input:
#   A CSV file containing a symmetric distance matrix
# Output:
#   Newick tree file
#   Optional tree plot
#
# Dependencies: ape
# ------------------------------------------------------------

library(ape)

# -----------------------------
# USER INPUT
# -----------------------------
distance_file <- "path/to/poppunk_distances.csv"   # <-- Edit here
output_tree   <- "path/to/poppunk_tree.nwk"        # <-- Edit here
outgroup_name <- "RW-TB008-LR-Asm"                 # L8 strain as outgroup
# -----------------------------

# Load symmetric distance matrix
distance_matrix <- read.csv(distance_file, row.names = 1, check.names = FALSE)

# Convert to dist object
dist_obj <- as.dist(distance_matrix)

# Build NJ tree
tree <- nj(dist_obj)

# Identify outgroup
out_idx <- which(tree$tip.label == outgroup_name)

if (length(out_idx) == 1) {
    tree_rooted <- root(tree, outgroup = out_idx)
    message("Tree successfully rooted at: ", outgroup_name)
} else {
    message("Outgroup not found. Returning unrooted tree.")
    tree_rooted <- tree
}

# Save tree to Newick format
write.tree(tree_rooted, file = output_tree)
message("Newick tree saved to: ", output_tree)

# Optional plot
plot(tree_rooted, show.tip.label = FALSE, no.margin = TRUE)
