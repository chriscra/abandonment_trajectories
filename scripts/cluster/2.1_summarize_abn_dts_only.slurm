#!/bin/bash
#SBATCH --job-name=2.1_summarize_abn_dts_only               # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=3:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=9  # 1-11                                      # array ID variables (i.e. like "i")
# SBATCH --mem-per-cpu=40G			                            # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_out_%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# load any required modules
module load gdal/2.2.4
module load geos/3.7.2

# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./2.1_summarize_abn_dts_only.R $SLURM_ARRAY_TASK_ID

# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_functions.R is up to date

# run with:
# sbatch --array [1,8,10] --mem-per-cpu=80G 2.1_summarize_abn_dts_only.slurm
# sbatch --array [2,4,5,7,11] --mem-per-cpu=60G 2.1_summarize_abn_dts_only.slurm
# sbatch --array [3,6,9] --mem-per-cpu=40G 2.1_summarize_abn_dts_only.slurm


# usage notes: _2021_03_13
# b (1)   nrow: 94,403,140.   ID: slurm_out_2_analyze_abn_33771701_1;   Time: 02:58:30; Memory Utilized: 112.38 GB (74.92% of 150.00 GB) 
# ^^^ weird message in the slurm_out "Warning message: system call failed: Cannot allocate memory"

# bh (2)  nrow: 74,231,964.   ID: slurm_out_2_analyze_abn_33771701_2;   Time: 01:30:19; Memory Utilized: 92.40 GB (61.60% of 150.00 GB)
# c (3)   nrow: 32,234,381.   ID: slurm_out_2_analyze_abn_33771302_3;   Time: 01:37:29; Memory Utilized: 42.19 GB (84.38% of 50.00 GB)
# g (4)   nrow: 59,898,990.   ID: slurm_out_2_analyze_abn_33771313_4;   Time: 02:18:05; Memory Utilized: 77.53 GB (81.61% of 95.00 GB)
# i (5)   nrow: 46,461,240.   ID: slurm_out_2_analyze_abn_33771702_5;   Time: 01:33:24; Memory Utilized: 69.44 GB; (60.38% of 115.00 GB)
# mg (6)  nrow: 38,256,075.   ID: slurm_out_2_analyze_abn_33771314_6;   Time: 00:44:23; Memory Utilized: 48.40 GB; (69.15% of 70.00 GB)
# n (7)   nrow: 66,156,714.   ID: slurm_out_2_analyze_abn_33771702_7;   Time: 01:16:23; Memory Utilized: 105.74 GB; (91.95% of 115.00 GB)
# o (8)   nrow: 101,661,520.  ID: slurm_out_2_analyze_abn_33771310_8;   Time: 03:47:28; Memory Utilized: 120.23 GB; (92.48% of 130.00 GB)
# s (9)   nrow: 22,573,740.   ID: slurm_out_2_analyze_abn_33771302_9;   Time: 00:57:38; Memory Utilized: 25.50 GB; (51.00% of 50.00 GB)
# v (10)  nrow: 93,444,494.   ID: slurm_out_2_analyze_abn_33771311_10;  Time: 03:03:29; Memory Utilized: 115.32 GB; (96.10% of 120.00 GB)
# w (11)  nrow: 70,874,020.   ID: slurm_out_2_analyze_abn_33771311_11;  Time: 01:23:04; Memory Utilized: 82.68 GB; (68.90% of 120.00 GB)



# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# Just make sure to turn off the array and mem per cpu variables
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory: this is really only necessary for 
# 2_analyze_abn.R, which processes all of the abandonment data in one fell swoop. 

# final memory allocation:
# sbatch --array [1,8,10] --mem-per-cpu=150G 2_analyze_abn.slurm
# sbatch --array [2,4,5,7,11] --mem-per-cpu=125G 2_analyze_abn.slurm
# sbatch --array [3,6,9] --mem-per-cpu=80G 2_analyze_abn.slurm




# upload to della
# scp /Users/christophercrawford/Google_Drive/_Projects/abandonment_trajectories/scripts/cluster/2.1* clc6@della:/home/clc6/abandonment_trajectories/scripts
# scp /Users/christophercrawford/Google_Drive/_Projects/abandonment_trajectories/scripts/util/_util_dt* clc6@della:/home/clc6/abandonment_trajectories/scripts/util
# scp /Users/christophercrawford/Google_Drive/_Projects/abandonment_trajectories/data_derived/site_df.csv clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived

# download from della with:
# scp clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/*result*2021_03_13* /Users/christophercrawford/Google_Drive/_Projects/abandonment_trajectories/data_derived/_2021_03_13
# scp clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/length_distill_df_2021_03_13.csv /Users/christophercrawford/Google_Drive/_Projects/abandonment_trajectories/data_derived/_2021_03_13
