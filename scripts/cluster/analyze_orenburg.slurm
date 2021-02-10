#!/bin/bash
#SBATCH --job-name=analyze_orenburg                       # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=5:59:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=8 # 2-11                                  # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=150G			                         # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_out_%x_%A_%a.txt                # "%x" = job name, "%A" = job ID, "%a" = array index.

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

# usage notes:
# Iraq test took about 1.5 hours and exactly 60 gb. Should give it 70 or 80 next time to be safe.
# The rest of the them took about 1.5 hours, and ~95 gb. 

# It looks like Orenburg finished successfully, given the slurm COMPLETED email message I got. 11/13/2020
# However, it gave a weird message at the end of the slurm.out, 
# "Warning message: 
#     system call failed: Cannot allocate memory "


# Inspection
# bh - checks out (ncell: 74231964)
# c - checks out (ncell: 32234381)
# g - checks out (ncell: 59898990)
# i - checks out (ncell: 46461240)
# mg - checks out (ncell: 38256075)
# n - checks out (ncell: 66156714)
# o - (ncell: 101661520) # failed, out of memory. may need something closer to 150 gb
# s - checks out (ncell: 22573740)
# v - checks out (ncell: 93444494)
# w - checks out (ncell: 70874020)
