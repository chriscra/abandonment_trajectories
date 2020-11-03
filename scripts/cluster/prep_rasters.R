# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, November 2nd, 2020

# Script to combine multiple individual year rasters into
# a multi-layer raster brick
# Steps include:
# 1) loading rasters
# 2) combining into a brick
# 3) updating cell values to match He's original coding
# 
# 4) writing brick to file.
# -------------------------------------------------------- #

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

site_list <- c("shaanxi", "belarus") # list of all sites
site <- site_list[indx] # set site:

# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "tidyverse", "rgdal",  "devtools")
cluster_dev_packages <- c("dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)
install_clpkg <- lapply(cluster_dev_packages, library, character.only = TRUE)

# load functions
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

print(paste0("Processing raw raster for ", site))

# run function
cc_r_to_dt(site = site, path = p_dat_derived)

