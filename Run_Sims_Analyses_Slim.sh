#!/bin/bash


echo "Provide the path to the working directory containing the SLiM script and output folders."
read dir

cd $dir/01_simulations

Rscript 00_run_simulations.R

cd ../2_analysis

Rscript 01_read_simulation_data.R

