#!/bin/bash
#SBATCH --job-name="save Belarus age dt to raster" 	# create a name for the job
#SBATCH --nodes=1 					# node count
#SBATCH --ntasks=1	 				# total number of tasks across all nodes
#SBATCH --cpus-per-task=1       			# cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=50G				# memory per cpu-core (4 GB is default)
#SBATCH --time=3:59:00 					# total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin				# send email when job begins
#SBATCH --mail-type=end					# send email when job ends
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/$SLURM_JOB_ID.out


cd /home/clc6/abandonment_trajectories/ # set the working directory

Rscript ./scripts/dt_age_to_raster.R

# Notes ----------------------------------
# Run this with 
# sbatch /home/clc6/abandonment_trajectories/scripts/dt_age_to_raster.slurm

