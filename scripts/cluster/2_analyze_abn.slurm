#!/bin/bash
#SBATCH --job-name=2_analyze_abn                          # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=6:59:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=150G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_out_%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# set working directory
cd /home/clc6/abandonment_trajectories/scripts

# r script passing array job number to the script
Rscript ./2_analyze_abn.R $SLURM_ARRAY_TASK_ID


# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_dt_filter_functions.R is up to date

# usage notes:
# Iraq test took about 1.5 hours and exactly 60 gb. Should give it 70 or 80 next time to be safe.
# The rest of the them took about 1.5 hours, and ~95 gb. 


# Inspection: test the memory usage of completed jobs with 
# $ seff <JobID>
# Initially, I ran this code for just 2-11 (excluding belarus). 
# Orenburg also failed, since it's even bigger than belarus, so I ran those two separately. 
# If I up the memory allocation to ~160 GB, all sites should run in an array.

# Site inspection.

# bh - checks out (ncell: 74,231,964). Used 90.57 GB, 1:28 hours. 
# c - checks out (ncell: 32,234,381). Used 39.58 GB, 0:53 hours.
# g - checks out (ncell: 59,898,990). Used 76.77 GB, 01:15:29 hours.
# i - checks out (ncell: 46,461,240). Used 63.15 GB, 00:52:25 hours.
# mg - checks out (ncell: 38,256,075). Used 46.44 GB, 00:31:14 hours.
# n - checks out (ncell: 66,156,714). Used 97.64 GB, 01:26:15 hours.
# o - failed, out of memory (ncell: 101,661,520). Used 109.96 GB (failed), 03:10:37 hours.
#### o - ran again, seems to have completed, despite weird message at the end of the slurm output:
#### looks like it used 145.22 GB, 03:38:41 hours.
# s - checks out (ncell: 22,573,740). Used 30.41 GB, 00:33:20 hours.
# v - checks out (ncell: 93,444,494). Used 115.60 GB, 03:03:51 hours.
# w - checks out (ncell: 70,874,020). Used 91.32 GB, 01:26:15 hours.

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# $ sbatch --array=1,3,5,7 or
# $ sbatch --array [1,2,3,4] testarray.sh
# $ sbatch --array [1-5,7,10] testarray.sh

# looks like you can also individually specify the memory when you call the slurm script. 