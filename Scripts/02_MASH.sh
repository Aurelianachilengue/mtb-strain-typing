#!/bin/bash
# Mash analyses used in the manuscript
# Unless otherwise stated, default parameters were used
#
# Input modes:
#  - Genome assemblies (FASTA)
#  - Paired-end reads (FASTQ), concatenated per sample

# ==========================================================
# Assembly-based analyses
# ==========================================================

# --------------------------
# Default parameters
# --------------------------

mash sketch \
  -o mash_default \
  assemblies/*.fasta

mash dist \
  mash_default.msh mash_default.msh > mash_dist_default.tsv

# --------------------------
# k-mer size = 31 (final setting)
# --------------------------

mash sketch \
  -k 31 \
  -s 100000 \
  -o mash_k31 \
  assemblies/*.fasta

mash dist \
  mash_k31.msh mash_k31.msh > mash_dist_k31.tsv


# ==========================================================
# Read-based analyses (paired-end reads)
# ==========================================================

# Concatenate R1 and R2 reads for each sample
# Format expected by Mash for read-based sketching

mkdir -p concatenated_reads

for r1 in *_R1.fastq.gz; do
  sample=${r1%_R1.fastq.gz}
  r2="${sample}_R2.fastq.gz"

  if [ -f "$r2" ]; then
    cat "$r1" "$r2" > "concatenated_reads/${sample}.fastq.gz"
  else
    echo "Warning: missing R2 read for ${sample}"
  fi
done

# Create list of concatenated read files
ls concatenated_reads/*.fastq.gz > reads.list.txt

# Sketch reads (k = 31, sketch size = 100,000)
mash sketch \
  -k 31 \
  -s 100000 \
  -l reads.list.txt \
  -o mash_reads_k31

# Compute distances
mash dist \
  mash_reads_k31.msh mash_reads_k31.msh > mash_dist_reads_k31.tsv
