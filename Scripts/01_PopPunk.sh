#!/bin/bash
# PopPUNK analysis used in the manuscript
# Unless otherwise stated, default parameters were used

# ==========================================================
# Input preparation
# ==========================================================

# Example: generate sample list file for genome assemblies
# Format: <sample_id> <TAB> <path_to_fasta>
# Paths shown here are illustrative

for fasta in assemblies/*.fasta; do
  [ -f "$fasta" ] && echo -e "$(basename "$fasta" .fasta)\t$fasta"
done > samples_paths.txt

# ----------------------------------------------------------
# Alternative input mode: paired-end reads
# Format: <sample_id> <TAB> <R1_path> <TAB> <R2_path>
# ----------------------------------------------------------

# Example only (used for simulated read analyses)

for r1_file in *_R1.fastq.gz; do
  sample_name=$(basename "$r1_file" | cut -d'_' -f1)
  r2_file="${r1_file/_R1/_R2}"
  echo -e "$sample_name\t$r1_file\t$r2_file"
done > samples_paths.txt

# ==========================================================
# PopPUNK analyses
# ==========================================================

# Default parameter run
poppunk \
  --create-db \
  --r-files samples_paths.txt \
  --output poppunk_db \
  --threads 8

poppunk_extract_distances.py \
  --distances poppunk_db/poppunk_db.dists \
  --output poppunk_distances.tsv

# Sensitivity test: k-mer range 21–41
poppunk \
  --create-db \
  --r-files samples_paths.txt \
  --output poppunk_k21_41_s100k \
  --sketch-size 100000 \
  --threads 8 \
  --min-k 21 \
  --max-k 41 \
  --k-step 4

poppunk_extract_distances.py \
  --distances poppunk_k21_41_s100k/poppunk_k21_41_s100k.dists \
  --output poppunk_k21_41_s100k_distances.tsv

# Final analysis: extended k-mer range 29–61
poppunk \
  --create-db \
  --r-files samples_paths.txt \
  --output poppunk_k29_61_s100k \
  --min-k 29 \
  --max-k 61 \
  --k-step 8 \
  --sketch-size 100000 \
  --threads 8

poppunk_extract_distances.py \
  --distances poppunk_k29_61_s100k/poppunk_k29_61_s100k.dists \
  --output poppunk_k29_61_s100k_distances.tsv
``
