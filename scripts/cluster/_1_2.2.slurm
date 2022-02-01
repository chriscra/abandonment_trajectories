#!/bin/bash
#SBATCH --job-name=run_1_thru_2.2                              # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=6:00:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                    # array ID variables (i.e. like "i")
# SBATCH --mem-per-cpu=150G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)


cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./1_prep_r_to_dt.R $SLURM_ARRAY_TASK_ID
Rscript ./2_analyze_abn.R $SLURM_ARRAY_TASK_ID
Rscript ./2.2_calc_recult_age.R $SLURM_ARRAY_TASK_ID

# ---------------------- #
# before running....
# sbatch _1_2.2.slurm


# # Send scripts
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./cluster/_1_2.2.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/_3_4.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/1_prep_r_to_dt.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/2_analyze_abn.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/2.1_summarize_abn_dts_only.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/2.2_calc_recult_age.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/3_distill_lengths.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# scp ./cluster/4_lc_of_abn.* clc6@della:/home/clc6/abandonment_trajectories/scripts/
# 
# # Send util scripts
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp ./util/_util_functions.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/
# scp ./util/_util_misc.R clc6@della:/home/clc6/abandonment_trajectories/scripts/util/


# $ seff <JobID>

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# Just make sure to turn off the array and mem per cpu variables
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory:

# # final memory allocation:
# sbatch --array [1,2,7,8,10] --mem-per-cpu=170G _1_2.2.slurm
# sbatch --array [4,5,11] --mem-per-cpu=130G _1_2.2.slurm
# sbatch --array [3,6] --mem-per-cpu=75G _1_2.2.slurm
# sbatch --array [9] --mem-per-cpu=45G _1_2.2.slurm
# sbatch --array [2] --mem-per-cpu=170G _1_2.2.slurm

# this test job used 30 GB, took 38 minutes



# download from della with:
# # make sure to replace run_label "_2022_01_31" #"_2021_03_13" # "_2021-03-05"
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/_2022_01_31
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/*result*_2022_01_31* ./
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/length_distill_df_2022_01_31.csv ./
# scp clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/recult_length_distill_df_2022_01_31.csv ./

# download slurm_out:
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp -r clc6@della:/home/clc6/abandonment_trajectories/scripts/slurm_out/ ./cluster


# seff results:

# 1, 8, 10 are still going, waiting on those. 
# _2 - out of memory
