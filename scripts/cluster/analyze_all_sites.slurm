#!/bin/bash
#SBATCH --job-name=analyze_iraq_test                          # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=3:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=5 # 2-11                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=60G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_out_%x_%A_%a.txt                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./analyze_all_sites.R $SLURM_ARRAY_TASK_ID


# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_dt_filter_functions.R is up to date
