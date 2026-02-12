
#!/usr/bin/env python3
import sys
import re
from collections import defaultdict
import dendropy
import numpy

# Script takes a tree and a clustering file as input
# Clustering file is a list of taxon ids per line where all in the same line are in a cluster
# All the taxa in a given cluster are mapped to the tree and the subtree that joins all these is extracted
# If this subtree is larger than the taxa listed in the cluster, it is likely a convergent clustering trait.
# Output is the trait, the cluster members and the number of members in the subtree
# Distances are also calculated on the tree between all taxa in a given cluster
# Output is the min, max, average distance along with the members (comma separated) and the cluster id
# note: only looks at traits with more than 1 taxon associated
# note: requires dendropy and numpy

# usage: python3 treePatternConvergent_groupsGiven.py treeFile clusteredFile

# get the tree name and file
try:
    tree_file = sys.argv[1]
except IndexError:
    print("\n tree file not supplied.")
    sys.exit()
try:
    patternF = open(sys.argv[2], 'r')
except IndexError:
    print("\n clustering file not supplied.")
    sys.exit()
except IOError:
    print("\n clustering file not found in directory.")
    sys.exit()

# create save file
try:
    save = open("treePatternConvergence.txt", 'w')
except IOError:
    print('no room for save file')
    sys.exit()

# read in the tree
tree = dendropy.Tree.get(path=tree_file, schema='newick', preserve_underscores=True)
taxa = tree.taxon_namespace
pdm = tree.phylogenetic_distance_matrix()

# for each id, get the taxa that are associated with it
patts = []
while True:
    s = patternF.readline()
    if not s:
        break
    s = s.rstrip()
    sections = s.split("\t")
    pattern = []
    for taxon in taxa:
        id = re.sub("^'", "", str(taxon))
        id = re.sub("'$", "", id)
        if id in sections:
            pattern.append(taxon)
    patts.append(pattern)
patternF.close()

# write the save header
save.write("taxa count\ttotal taxa in pattern subtree\tmin dist\tmean dist\tmax dist\ttaxon labels\n")

# for each cluster get the subtree and count the members of it
for pattern in patts:
    treeDist = []
    if len(pattern) > 1:
        mrca = tree.mrca(taxa=pattern)
        leaves = mrca.leaf_nodes()
        for tax1N in range(len(pattern)):
            tax1 = pattern[tax1N]
            for tax2N in range(tax1N + 1, len(pattern)):
                tax2 = pattern[tax2N]
                dist = pdm(tax1, tax2)
                treeDist.append(dist)
        save.write(f"{len(pattern)}\t{len(leaves)}\t{min(treeDist)}\t{numpy.mean(treeDist)}\t{max(treeDist)}\t")
        ids = []
        for taxon in pattern:
            id = re.sub("^'", "", str(taxon))
            id = re.sub("'$", "", id)
            ids.append(id)
        save.write(",".join(ids) + "\n")
save.close()
sys.exit()
