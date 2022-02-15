#!/bin/bash
#SBATCH --job-name=feb_3_4                               # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=4:00:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                    # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=80G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./3_distill_lengths.R
Rscript ./4_lc_of_abn.R

# ---------------------- #
# before running....
# sbatch _3_4.slurm


# # Send scripts
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./cluster/_3_4.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/3_distill_lengths.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/4_lc_of_abn.* clc6@della:/home/clc6/abandonment_trajectories/scripts/

# # Send util scripts
# scp ./util/_util_functions.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/
# scp ./util/_util_misc.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/

# ------------------ usage notes ------------------ #
# $ seff <JobID>
# Job ID: 39547461
# Cluster: della
# User/Group: clc6/spia
# State: COMPLETED (exit code 0)
# Cores: 1
# CPU Utilized: 00:07:43
# CPU Efficiency: 97.89% of 00:07:53 core-walltime
# Job Wall-clock time: 00:07:53
# Memory Utilized: 17.27 GB
# Memory Efficiency: 21.59% of 80.00 GB
# ------------------ ----------- ------------------ #

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# Just make sure to turn off the array and mem per cpu variables
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory:

# final memory allocation:
# sbatch --array [1,8,10] --mem-per-cpu=150G _1_2.2.slurm
# sbatch --array [2,4,5,7,11] --mem-per-cpu=125G _1_2.2.slurm
# sbatch --array [3,6,9] --mem-per-cpu=80G _1_2.2.slurm
# sbatch --array [9] --mem-per-cpu=100G _1_2.2.slurm

# download from della with:
# # make sure to replace run_label "_2022_01_31" #"_2021_03_13" # "_2021-03-05"
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/_2022_01_31
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/*result*_2022_01_31* ./
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/length_distill_df_2022_01_31.csv ./
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/recult_length_distill_df_2022_01_31.csv ./
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/abn_*_2022_01_31* ../


# download slurm_out:
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp -r clc6@della:/home/clc6/abandonment_trajectories/scripts/slurm_out/ ./cluster

