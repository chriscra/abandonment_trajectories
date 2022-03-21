# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, January 28th, 2022

# Script to calculate how long periods of recultivation last, following qualifying periods 
# of abandonment for all sites, producing data.tables for further analysis.
# -------------------------------------------------------- #

# Calculate recultivation lengths:

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      # "landscapemetrics", "landscapetools", "sp",
                      "sf", "tidyverse", "rgdal", "dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)


# set paths:
p_dat_derived   <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/gpfs/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/gpfs/clc6/abandonment_trajectories/raw_rasters/"
p_tmp           <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/tmp/"

# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# source functions:
source("/home/clc6/abandonment_trajectories/scripts/_util/_util_functions.R")



# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# set up parameters:
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
site <- site_df$site[indx] # set site
site_label <- site_df$label[indx] # set label

# run_label (time stamp)
run_label <- format(Sys.time(), "_%Y_%m_%d") 


cat(fill = TRUE, "Set up site parameters:")
print(t(site_df[indx, ]))

cat(fill = TRUE, "Run label (time stamp):", run_label)
# load required data.tables


# ----- 0. Start - load the original 1s and 0s data.table (recoded) ----- 
dt_recult_age <-  fread(input = paste0(p_input_rasters, site, "_binary", run_label, ".csv"))
dt_age <-         fread(input = paste0(p_input_rasters, site, "_age",    run_label, ".csv"))
dt_diff <-        fread(input = paste0(p_input_rasters, site, "_diff",   run_label, ".csv"))

# ----- 1. Run the recultivation age calculator function, specifying abn_threshold, and save to file. ----- 
# 1.1 Run recult_age function:
cat(fill = TRUE, "Run cc_calc_recult_age() function, calculating the age of recultivated lands.")
tic("Run cc_calc_recult_age() (dt_recult_age)")
dt_recult_age <- cc_calc_recult_age(dt = dt_recult_age, 
                                    dt_age = dt_age,
                                    dt_diff = dt_diff,
                                    abn_threshold = 5)
toc(log = TRUE)


# 1.2. Write out cleaned recultivation age data.table
cat(fill = TRUE, "Write out cleaned recultivation age data.table to:", 
    paste0(p_input_rasters, site, "_recult_age", run_label,".csv"))
tic("wrote out cleaned recultivation age data.table (dt_recult_age)")
fwrite(dt_recult_age, file = paste0(p_input_rasters, site, "_recult_age", run_label,".csv"))
toc(log = TRUE)


# ----- 2. Calculate recultivation diff data.table, to use to extract lengths, and save to file. ----- 
# Produces a data.table with year-to-year lagged differences (each year subtracted by the previous year, much like base::diff())

# 2.1. Run diff function
cat(fill = TRUE, "Make dt_diff, with year-to-year lagged differences: cc_diff_dt()")
tic("made dt_recult_diff")
dt_recult_diff <- cc_diff_dt(dt_recult_age)
toc(log = TRUE)

# 2.2. Write out dt_recult_diff
cat(fill = TRUE, "Write out dt_recult_diff to:", 
    paste0(p_input_rasters, site, "_recult_diff", run_label,".csv"))
tic("write out dt_diff")
fwrite(dt_recult_diff, file = paste0(p_input_rasters, site, "_recult_diff", run_label,".csv"))
toc(log = TRUE)


# ----- 3. Extract the length of each period of recultivation following a qualifying period of abandonment, turn into a data.table, and save to file ----- 

# 3.1. Extract length.
cat(fill = TRUE, "Extract the length of each period of recultivation: cc_extract_length()")
tic("extracted length")
# note that this function can be easily modified to search for positive or negative numbers, if multiplying the
# data.tables by -1 takes a long time. 
recult_length <- cc_extract_length(dt_recult_diff, length_type = "recultivation")
toc(log = TRUE)

# 3.2. Create a data.table listing the lengths.
cat(fill = TRUE, "Create a data.table listing the recultivations lengths, write to:", 
    paste0(p_input_rasters, site, "_recult_length", run_label,".csv"))
tic("created data.table of recultivation lengths")
recult_length <- data.table(length = recult_length)
toc(log = TRUE)

# 3.3. Write to file.
tic("wrote out length data.table")
fwrite(recult_length, file = paste0(p_input_rasters, site, "_recult_length", run_label,".csv"))
toc(log = TRUE)

