#!/bin/bash
#SBATCH --job-name="Remove 0s; Belarus"                   # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=12:00:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --mem-per-cpu=200G			                            # memory per cpu-core (4 GB is default). Can alternatively set total memory with the command '--mem=61G'. Make sure not to use both
#SBATCH --mail-type=begin			                            # send email when job begins
#SBATCH --mail-type=end				                            # send email when job ends
#SBATCH --mail-user=clc6@princeton.edu

module load gdal # this line loads gdal, which is required for sf

Rscript /home/clc6/abandonment_trajectories/scripts/remove_0s_b.R

# Notes ----------------------------------
# Run this with 
# sbatch /home/clc6/abandonment_trajectories/scripts/remove_0s_b.slurm

