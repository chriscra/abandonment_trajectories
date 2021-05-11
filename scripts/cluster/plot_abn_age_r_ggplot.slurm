#!/bin/bash
#SBATCH --job-name=plot_abn_age_r_ggplot                      # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=1:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                     # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=100G			                            # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_out_%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index


echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# run R script
Rscript ./plot_abn_age_r_ggplot.R

# run with:
# sbatch plot_abn_age_r_ggplot.slurm
