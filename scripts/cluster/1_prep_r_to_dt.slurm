#!/bin/bash
#SBATCH --job-name=1_prep_r_to_dt                         # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=3:59:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=160G			                          # memory per cpu-core (4 GB is default).
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
Rscript ./1_prep_r_to_dt.R $SLURM_ARRAY_TASK_ID


# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_functions.R is up to date

# usage notes:
# $ seff <JobID>

# Times and memory allocations for "2_analyze_abn.R". Not sure what these will be in practice

# b (1)   nrow: 94,403,140.   ID: _1;   Time: 02:45:57; Memory Utilized: 118.08 GB (78.72% of 150.00 GB)
# bh (2)  nrow: 74,231,964.   ID: _2;   Time: 01:29:13; Memory Utilized: 94.77 GB (63.18% of 150.00 GB)
# c (3)   nrow: 32,234,381.   ID: _3;   Time: 00:50:09; Memory Utilized: 38.02 GB (25.35% of 150.00 GB)
# g (4)   nrow: 59,898,990.   ID: _4;   Time: 01:13:27; Memory Utilized: 77.47 GB (51.64% of 150.00 GB)
# i (5)   nrow: 46,461,240.   ID: _5;   Time: 00:55:23; Memory Utilized: 56.50 GB; (37.67% of 150.00 GB)
# mg (6)  nrow: 38,256,075.   ID: _6;   Time: 00:31:59; Memory Utilized: 49.19 GB; (32.80% of 150.00 GB)
# n (7)   nrow: 66,156,714.   ID: _7;   Time: 01:29:08; Memory Utilized: 81.75 GB; (54.50% of 150.00 GB)
# o (8)   nrow: 101,661,520.  ID: _8;   Time: 03:25:43; Memory Utilized: 113.51 GB; (75.67% of 150.00 GB)
# s (9)   nrow: 22,573,740.   ID: _9;   Time: 00:33:41; Memory Utilized: 27.89 GB; (18.59% of 150.00 GB)
# v (10)  nrow: 93,444,494.   ID: _10;  Time: 02:52:24; Memory Utilized: 103.15 GB; (68.77% of 150.00 GB)
# w (11)  nrow: 70,874,020.   ID: _11;  Time: 01:33:15; Memory Utilized: 109.50 GB; (73.00% of 150.00 GB)

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory:

# sbatch --array [1,8] --mem-per-cpu=130G 1_prep_r_to_dt.slurm
# sbatch --array [10,11] --mem-per-cpu=120G 1_prep_r_to_dt.slurm
# sbatch --array [2] --mem-per-cpu=110G 1_prep_r_to_dt.slurm
# sbatch --array [4,7] --mem-per-cpu=95G 1_prep_r_to_dt.slurm
# sbatch --array [5,6] --mem-per-cpu=70G 1_prep_r_to_dt.slurm
# sbatch --array [3,9] --mem-per-cpu=50G 1_prep_r_to_dt.slurm
