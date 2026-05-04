#!/bin/bash
# SKA2 analyses used in the manuscript
# Unless otherwise stated, default parameters were used
#
# Default split k-mer size for SKA2: k = 17
# Tested split k-mer sizes: 31, 41, 51
# k = 31 was selected for downstream analyses

# ==========================================================
# A) Input from genome assemblies (FASTA)
# ==========================================================
# Expected input files: SAMPLE.fasta

for f in *.fasta; do
  sample=$(basename "$f" .fasta)
  echo -e "$sample\t$f"
done > tab_assemblies.txt

# ---- Default SKA2 run (k = 17) ----
ska build -o ska2_default -f tab_assemblies.txt
ska distance ska2_default.skf > ska2_default_distances.tsv

# ---- Tested k-mer sizes ----
ska build -k 31 -o ska2_k31 -f tab_assemblies.txt
ska distance ska2_k31.skf > ska2_k31_distances.tsv

ska build -k 41 -o ska2_k41 -f tab_assemblies.txt
ska distance ska2_k41.skf > ska2_k41_distances.tsv

ska build -k 51 -o ska2_k51 -f tab_assemblies.txt
ska distance ska2_k51.skf > ska2_k51_distances.tsv


# ==========================================================
# B) Alternative input: paired-end reads (optional)
# ==========================================================
# Expected input files:
#   SAMPLE_R1.fastq.gz
#   SAMPLE_R2.fastq.gz

for r1 in *_R1.fastq.gz; do
  sample=$(basename "$r1" | cut -d'_' -f1)
  r2="${r1/_R1/_R2}"
  echo -e "$sample\t$r1\t$r2"
done > tab_reads.txt

# Example SKA2 run on reads (k = 31)
ska build -k 31 -o ska2_reads_k31 -f tab_reads.txt
ska distance ska2_reads_k31.skf > ska2_reads_k31_distances.tsv
