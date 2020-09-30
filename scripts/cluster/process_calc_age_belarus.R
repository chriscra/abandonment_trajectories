# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, August 27th, 2020

# Script to process raw land cover raster in order to
# calculate the length of agricultural abandonment periods.
# Steps include:
# 1) converting to data.table, 2) filtering to just abandonment cells, 
# 3) calculating age, and 4) extracting lengths, all the while writing out files.
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "raster", "rgdal", "sp", "tictoc", "devtools")
cluster_dev_packages <- c("dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE) # load them
install_clpkg <- lapply(cluster_dev_packages, library, character.only = TRUE) # load them


# set paths
file_in <- "/scratch/network/clc6/abandonment_trajectories/data_derived/belarus.tif"
path_out <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

# load functions
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# run function
cc_process_rasters(input_raster_file = file_in,
                   name = "belarus", 
                   path_out = path_out,
                   gsub_pattern = "smolensk")
