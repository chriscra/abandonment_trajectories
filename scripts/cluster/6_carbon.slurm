#!/bin/bash
#SBATCH --job-name=6_carbon                     # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=1:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                     # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=100G			                            # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index


echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)


# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# run R script
Rscript ./6_carbon.R

# run with:
# sbatch 6_carbon.slurm


# send scripts:
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./cluster/6_carbon.slurm clc6@della:/home/clc6/abandonment_trajectories/scripts/