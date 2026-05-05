#!/bin/bash
# DNAdiff analyses used in the manuscript
# Unless otherwise stated, default parameters were used
#
# DNAdiff default:
# - Whole-genome alignment using nucmer
# - SNPs required to be flanked by ≥20 bp exact matches

# ==========================================================
# Input: genome assemblies (FASTA)
# ==========================================================
# Expected input files: SAMPLE.fasta
# One genome per FASTA (multi-contig allowed)

mkdir -p dnadiff_results

# ----------------------------------------------------------
# Pairwise all-vs-all DNAdiff comparisons
# ----------------------------------------------------------

for g1 in *.fasta; do
  for g2 in *.fasta; do

    # avoid self-comparisons and duplicate pairs
    if [[ "$g1" < "$g2" ]]; then

      s1=$(basename "$g1" .fasta)
      s2=$(basename "$g2" .fasta)

      echo "Running DNAdiff: $s1 vs $s2"

      dnadiff "$g1" "$g2" \
        -p dnadiff_results/${s1}_vs_${s2}

    fi
  done
done
