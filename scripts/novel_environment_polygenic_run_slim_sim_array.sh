#!/bin/bash
#SBATCH -J slim_array-%j
#SBATCH -t 0-2:00
#SBATCH --mem 8000
#SBATCH -o tmp/slim_RE-%A_%a.out

# launches a job array of recombination rate evolution simulations
# used by Rscript launcher to launch arrays for each paramter combination
# individual arrays can be run like:
# sbatch --array=0-3 scripts/run_slim_sim_array.sh p1si=1000 p2si=1000 mu=0.00000175 rr=0.00003075 rrc=1.0 rrcd=1 chrlen=100000 samp=1000 mig=0.5 outfold=data/slim_output/1000_1000_0.00000175_0.00003075_1_1_100000_1000_0.5/

echo "RUNNING SLIM SIMULATION WITH ARRAY INDEX ${SLURM_ARRAY_TASK_ID}"

# parse named arguments
# the array syntax does something weird to numbered arguments
# so this is the workaround
for ARGUMENT in "$@"
do

    KEY=$(echo $ARGUMENT | cut -f1 -d=)
    VALUE=$(echo $ARGUMENT | cut -f2 -d=)   

    case "$KEY" in
            migrate)     migrate=${VALUE} ;;
            npops)       npops=${VALUE} ;;
            popsi)       popsi=${VALUE} ;;
            invssup)     invssup=${VALUE} ;;
            sadaptive)   sadaptive=${VALUE} ;;
            mutrate)     mutrate=${VALUE} ;;
            recombrate)  recombrate=${VALUE} ;;
            ndivselloci)  ndivselloci=${VALUE} ;;
            outfold)     outfold=${VALUE} ;;
            *)   
    esac    

done

# make the base  output directory 
mkdir -p $outfold

slim -d migration_rate=$migrate \
-d numpops=$npops \
-d pop_size=$popsi \
-d inversions_supress_recomb=$invssup \
-d mutation_rate=$mutrate \
-d recombination_rate=$recombrate \
-d n_div_sel_loci=$ndivselloci \
-d s_adaptive=$sadaptive \
-d output_folder=\'${outfold}\' \
slim/inversion_constraint_three_pop_polygenic.slim