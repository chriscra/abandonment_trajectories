#!/bin/bash
#SBATCH --job-name=5_potential_age                          # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=5:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
# SBATCH --mem-per-cpu=150G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)


# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./5_potential_age_without_recultivation.R $SLURM_ARRAY_TASK_ID
# Rscript ./5_potential_age_loop.R $SLURM_ARRAY_TASK_ID



# # Send scripts
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./cluster/5_potential_age_without_recultivation.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/5_potential_age_loop.R clc6@della:/home/clc6/abandonment_trajectories/scripts/
# 
# # Send util scripts
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./util/_util_functions.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/
# scp ./util/_util_misc.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/


# download slurm_out:
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp clc6@della:/home/clc6/abandonment_trajectories/scripts/slurm_out/5_potential_age* /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/cluster/slurm_out


# -------------------- #

# # final memory allocation:
# sbatch --array [1,2,7,8,10] --mem-per-cpu=170G 5_potential_age_without_recultivation.slurm
# sbatch --array [4,5,11] --mem-per-cpu=130G 5_potential_age_without_recultivation.slurm
# sbatch --array [3,6] --mem-per-cpu=75G 5_potential_age_without_recultivation.slurm
# sbatch --array [9] --mem-per-cpu=45G 5_potential_age_without_recultivation.slurm

# first run:
#      job_id array_id    time     mem_used  mem_given state                          array mem_used_p
#       <dbl> <chr>       <chr>    <chr>         <dbl> <chr>                          <dbl> <chr>     
#  1 39599045 39599041_1  00:18:34 143.88 GB       170 State: COMPLETED (exit code 0)     1 84.64%    
#  2 39599046 39599041_2  00:13:39 97.83 GB        170 State: COMPLETED (exit code 0)     2 57.55%    
#  3 39599055 39599043_3  00:06:16 49.32 GB         75 State: COMPLETED (exit code 0)     3 65.76%    
#  4 39599049 39599042_4  00:10:08 75.98 GB        130 State: COMPLETED (exit code 0)     4 58.45%    
#  5 39599054 39599042_5  00:00:04 24.00 KB        130 State: FAILED (exit code 1)        5 0.00%     
#  6 39599043 39599043_6  00:00:04 100.00 KB        75 State: FAILED (exit code 1)        6 0.00%     
#  7 39599047 39599041_7  00:10:23 83.10 GB        170 State: COMPLETED (exit code 0)     7 48.88%    
#  8 39599048 39599041_8  00:20:27 150.76 GB       170 State: COMPLETED (exit code 0)     8 88.68%    
#  9 39599044 39599044_9  00:00:04 68.00 KB         45 State: FAILED (exit code 1)        9 0.00%     
# 10 39599041 39599041_10 00:19:04 154.16 GB       170 State: COMPLETED (exit code 0)    10 90.68%    
# 11 39599042 39599042_11 00:00:04 88.00 KB        130 State: FAILED (exit code 1)       11 0.00%     

# second run:
# sbatch --array [5,6,9,11] --mem-per-cpu=130G 5_potential_age_without_recultivation.slurm

