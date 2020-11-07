#!/bin/bash
#SBATCH --job-name=process_rasters_full                          # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=11:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=1-11                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=60G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./r_to_dt_array.R $SLURM_ARRAY_TASK_ID
Rscript ./filter_abn_dt.R $SLURM_ARRAY_TASK_ID
Rscript ./summarize_abn_dts.R $SLURM_ARRAY_TASK_ID
Rscript ./calc_max_age.R $SLURM_ARRAY_TASK_ID
