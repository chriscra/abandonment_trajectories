#!/bin/bash
#SBATCH --job-name="process Belarus raster to calc age" # create a name for the job
#SBATCH --nodes=1 					# node count
#SBATCH --ntasks=1	 				# total number of tasks across all nodes
#SBATCH --cpus-per-task=1       			# cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=60G				# memory per cpu-core (4 GB is default)
#SBATCH --time=8:00:00 					# total run time limit (HH:MM:SS)
# SBATCH --mem=61G 					# total memory, as an alternative (but make sure to only use one memory command)
#SBATCH --mail-type=begin				# send email when job begins
#SBATCH --mail-type=end					# send email when job ends
#SBATCH --mail-user=clc6@princeton.edu

# module load gdal # this line loads gdal, which is required for sf

cd /home/clc6/abandonment_trajectories/slurm_out/ # set the working directory. This should be where the slurm output goes

Rscript /home/clc6/abandonment_trajectories/scripts/process_calc_age_belarus.R

# Notes ----------------------------------
# Run this with 
# sbatch /home/clc6/abandonment_trajectories/scripts/process_calc_age_belarus.slurm

