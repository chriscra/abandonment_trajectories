#!/bin/bash
#SBATCH --job-name="max age" 				# create a name for the job
#SBATCH --nodes=1 					# node count
#SBATCH --ntasks=1	 				# total number of tasks across all nodes
#SBATCH --cpus-per-task=1       			# cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=20G				# memory per cpu-core (4 GB is default)
#SBATCH --time=2:00:00 					# total run time limit (HH:MM:SS)
#SBATCH --mail-type=begin				# send email when job begins
#SBATCH --mail-type=end					# send email when job ends
#SBATCH --mail-user=clc6@princeton.edu



cd /home/clc6/abandonment_trajectories/ # set the working directory

Rscript ./scripts/calc_max_age.R

