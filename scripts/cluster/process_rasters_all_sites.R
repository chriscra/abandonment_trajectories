# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, November 6th, 2020

# Script to process rasters for all sites, doing the following:
# 1. 
# -------------------------------------------------------- #

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])


# set up parameters:
# list of all sites
site_list <- sort(
  c("shaanxi", "belarus", 
    "chongqing", "goias", "mato_grosso",
    "nebraska", "wisconsin", "volgograd",
    "orenburg", "bosnia_herzegovina", "iraq") 
)

site_label_list <- sort(
  c("_s", "_b", 
    "_c", "_g", "_mg", 
    "_n", "_w", "_v", 
    "_o", "_bh", "_i")
)

site <- site_list[indx] # set site:
site_label <- site_label_list[indx] # set label

blip_label <- "_blip1"
label <- NULL # for calculating the max age

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input <- paste0(p_dat_derived, "input_rasters/")
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# source functions:
source("/home/clc6//abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")



# 1. Convert raw rasters into data.tables
print(paste0("Converting raw rasters to data.tables: ", site))
cc_r_to_dt(site = site, path = p_input)

# 2. Process raw data.tables in order to calculate the length of agricultural abandonment periods.
      # Steps include:
      # 1) filtering to just abandonment cells, 
      # 2) filling recultivation blips based on a threshold,
      # 3) calculating age, and 4) extracting lengths, all the while writing out files.

print(paste0("Filter data.tables for abandonment raw rasters to data.tables: ", site))

cc_filter_abn_dt(site = site,
                 path = p_input,
                 label = blip_label,
                 clean_blips = TRUE)

# 3 Calculate maximum age, serial
cc_calc_max_age(directory = p_input, 
                site = site, 
                blip_label = blip_label,
                label = label) 


# 4. Summarize the abandonment datatables into dataframes for plotting purposes
print(paste0("Summarizing abandonment data.tables for: ", site))

cc_summarize_abn_dts(
  input_path = p_input,
  site = site,
  blip_label = blip_label,
  outfile_label = paste0(blip_label, site_label),
  abandonment_threshold = 5,
  include_all = TRUE
)


