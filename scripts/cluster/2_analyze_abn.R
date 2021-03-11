# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 4th, 2021

# Script to analyze abandonment trajectories rasters for all sites
# -------------------------------------------------------- #
# Note: this is the primary script to analyze abandonment trajectories in the
# land cover rasters from Yin et al. 2020. It contains 4 steps, starting 
# with the prepped data.tables resulting from "1_prep_r_to_dt.R" and producing 
# various results data.tables, which are further synthesized into results data.frames.

# Steps include:
# 1. cc_filter_abn_dt() // Process raw data.tables in order to calculate the length of agricultural abandonment periods.
    # Steps include:
    # 1) filtering to just abandonment cells, 
    # 2) filling recultivation blips based on a threshold,
    # 3) calculating age, and 
    # 4) extracting lengths, all the while writing out files.
# 2. cc_calc_max_age() // Calculate maximum age, in serial.
# 3. cc_save_dt_as_raster() // Save various data.tables as rasters.
# 4. cc_summarize_abn_dts() // Summarize the abandonment datatables into dataframes for plotting purposes.

# SLURM Scripts that accompany this are:
# analyze_all_sites.slurm, which runs for all sites except belarus (1), which requires a greater memory allocation.
# orenburg was close, but I was able to make it work with 150 gb. total.


# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived   <-    "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/network/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/network/clc6/abandonment_trajectories/raw_rasters/"


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")



# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# site_list <- c("belarus", "bosnia_herzegovina", "chongqing", 
#                "goias", "iraq", "mato_grosso", 
#                "nebraska", "orenburg", "shaanxi", 
#                "volgograd", "wisconsin")
# 
# site_label_list <- c("_b", "_bh", "_c", 
#                      "_g", "_i", "_mg", 
#                      "_n", "_o", "_s", 
#                      "_v", "_w")


site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label

# time stamp
time_stamp <- paste0("_", Sys.Date()) # format(Sys.time(), "_%Y-%m-%d_%H%M%S")
run_label <- paste0(#site_label, 
                    time_stamp)



cat(fill = TRUE, "Set up site parameters:")
print(t(site_df[indx, ]))

cat(fill = TRUE, "Run label (time stamp):", run_label)



# -------------------------------------------------------- #
# 1. Process raw data.tables in order to calculate the length of agricultural abandonment periods. ----
      # Steps include:
      # 1) filtering to just abandonment cells, 
      # 2) filling recultivation blips based on a threshold,
      # 3) calculating age, and 4) extracting lengths, all the while writing out files.

cat("1. Filter data.tables for abandonment raw rasters to data.tables: ", site, fill = TRUE)


cc_filter_abn_dt(site = site,
                 path = p_input_rasters,
                 run_label = run_label,
                 pass_temporal_filter = TRUE, filter_edge = TRUE,
                 temporal_filter_replacement_value = 1)


# -------------------------------------------------------- #
# 2. Calculate maximum age, serial ----
cat("2. Calculate maximum age, in serial:", site, fill = TRUE)
cc_calc_max_age(directory = p_input_rasters,
                site = site,
                run_label = run_label)


# -------------------------------------------------------- #
# 3. save various data.tables as rasters ----
cat("3. Save data.tables as rasters: ", site, fill = TRUE)

dt_ids <- c(
  "",  # the re-coded land cover .csv
  paste0("_max_age", run_label),  # can also include the max age raster, perhaps?
  paste0("_age", run_label) # the abandonment age .csv
)

cat("types of data.tables to save as rasters:", fill = TRUE)
print(dt_ids)

for (i in dt_ids) {
  cc_save_dt_as_raster(site = site,
                       type = i,
                       input_path = p_input_rasters,
                       output_path = p_input_rasters)
}

# cc_save_dt_as_raster(site = site, 
#                      type = "", 
#                      input_path = p_input_rasters, 
#                      output_path = p_input_rasters)
# 
# cc_save_dt_as_raster(site = site, 
#                      type = paste0("_age", run_label), 
#                      input_path = p_input_rasters, 
#                      output_path = p_input_rasters)
# 
# cc_save_dt_as_raster(site = site, 
#                      type = paste0("_max_age", run_label), 
#                      input_path = p_input_rasters, 
#                      output_path = p_input_rasters)



# -------------------------------------------------------- #
# 4. Summarize the abandonment datatables into dataframes for plotting purposes ----
cat("4. Summarizing abandonment data.tables: ", site, fill = TRUE)

cc_summarize_abn_dts(
  input_path = p_input_rasters,
  output_path = p_input_rasters, # could also be p_output, this is just where the .rds goes.
  site = site,
  run_label = run_label,
  abandonment_threshold = 5,
  include_all = TRUE
)


