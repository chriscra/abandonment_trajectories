# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, November 11th, 2020

# Script to process rasters for all sites
# -------------------------------------------------------- #

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])


# set up parameters:
# list of all sites
site_list <- c("belarus", "bosnia_herzegovina", "chongqing", 
               "goias", "iraq", "mato_grosso", 
               "nebraska", "orenburg", "shaanxi", 
               "volgograd", "wisconsin")


site_label_list <- c("_b", "_bh", "_c", 
                     "_g", "_i", "_mg", 
                     "_n", "_o", "_s", 
                     "_v", "_w")

site <- site_list[indx] # set site:
site_label <- site_label_list[indx] # set label

blip_label <- "_b1"
label <- NULL # for calculating the max age

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <-      "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output <-           "/scratch/network/clc6/abandonment_trajectories/output/"
p_raw_rasters_path <- "/scratch/network/clc6/abandonment_trajectories/raw_rasters/"


# source functions:
source("/home/clc6//abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# load site_df
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))




# 0. Merge raw raster layers
cat("0. Merge raw raster layers", fill = TRUE)
tic("Merge rasters")
cc_merge_rasters(site = site, site_df = site_df, input_path = p_raw_rasters_path)
toc(log = TRUE)

# 1. Convert raw rasters into data.tables (including renaming and recoding)
cat("1. Converting raw rasters into data.tables (including renaming and recoding): ", site, fill = TRUE)
cc_r_to_dt(site = site, 
           input_path = p_raw_rasters_path, 
           output_path = p_input_rasters, 
           site_df = site_df)

# 2. Process raw data.tables in order to calculate the length of agricultural abandonment periods.
      # Steps include:
      # 1) filtering to just abandonment cells, 
      # 2) filling recultivation blips based on a threshold,
      # 3) calculating age, and 4) extracting lengths, all the while writing out files.

cat("2. Filter data.tables for abandonment raw rasters to data.tables: ", site, fill = TRUE)

cc_filter_abn_dt(site = site,
                 path = p_input_rasters,
                 blip_label = blip_label,
                 clean_blips = TRUE)

# 3 Calculate maximum age, serial
cat("3. Calculate maximum age, in serial:", site, fill = TRUE)
cc_calc_max_age(directory = p_input_rasters, 
                site = site, 
                blip_label = blip_label,
                label = label) 


# 4. save various data.tables as rasters:
cat("4. Saving data.tables as rasters: ", site, fill = TRUE)

dt_types <- c(
  "", 
  paste0("_age", blip_label)
)

cat("types of data.tables to save as rasters:", fill = TRUE)
print(dt_types)

for (i in dt_types) {
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
#                      type = paste0("_age", blip_label), 
#                      input_path = p_input_rasters, 
#                      output_path = p_input_rasters)



# 5. Summarize the abandonment datatables into dataframes for plotting purposes
cat("5. Summarizing abandonment data.tables: ", site, fill = TRUE)

cc_summarize_abn_dts(
  input_path = p_input_rasters,
  site = site,
  blip_label = blip_label,
  outfile_label = paste0(blip_label, site_label),
  abandonment_threshold = 5,
  include_all = TRUE
)



