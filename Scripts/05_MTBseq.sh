#!/bin/bash
# MTBseq analysis used in the manuscript
# Standard MTBseq pipeline was used without modification
# Reference genome: H37Rv (NC_000962.3)
# For full implementation details see:
# Kohl et al., 2018 (PeerJ) and MTBseq manual

# ==========================================================
# Input: paired-end Illumina reads
# ==========================================================
# Expected input files:
#   SAMPLE_R1.fastq.gz
#   SAMPLE_R2.fastq.gz

# Example MTBseq full pipeline run
MTBseq --step TBfull --threads 8
