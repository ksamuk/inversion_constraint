#!/bin/bash

#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH -J slimsim
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=ksamuk@gmail.com
#SBATCH -o tmp/script-%j.out

Rscript 01_run_simulations.R


