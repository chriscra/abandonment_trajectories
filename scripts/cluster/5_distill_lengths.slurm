#!/bin/bash
#SBATCH --job-name=5_distill_lengths                      # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=1:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                     # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=60G			                            # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index


echo "Executing on the machine:" $(hostname)


# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# run R script
Rscript ./5_distill_lengths.R

# run with:
# sbatch 5_distill_lengths.slurm
