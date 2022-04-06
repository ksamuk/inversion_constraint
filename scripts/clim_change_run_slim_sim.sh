#!/bin/bash
#SBATCH -J slim_RE-%j
#SBATCH -o tmp/slim_RE/slim_RE-%j.out

# launches a single replicate of the recombination rate evolution simulation
# used by Rscript launcher to launch many replicates of each paramter combination
# individual sims can be run like:
# sh scripts/run_slim_sim 1000 1000 1.75e-06 3.075e-05 0.5 1 1e+05 1000 0.001 data/slim_output/1000_1000_1.75e-06_3.075e-05_0.5_1_1e+05_1000_0.001/

slim -d migration_rate=$1 \
-d numpops=$2 \
-d pop_size=$3 \
-d inversions_supress_recomb=$4 \
-d sel_coeff_adaptive_allele=$5 \
-d mutation_rate=$6 \
-d recombination_rate=$7 \
-d output_folder=\'${8}\' \
slim/inversion_constraint_three_pop.slim