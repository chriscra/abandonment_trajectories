# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 27th, 2020

# Script to process raw data.tables in order to
# calculate the length of agricultural abandonment periods.

# Steps include:
# 1) filtering to just abandonment cells, 
# 2) filling recultivation blips based on a threshold,
# 3) calculating age, and 4) extracting lengths, all the while writing out files.
# -------------------------------------------------------- #

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

site_list <- c("shaanxi", "belarus") # list of all sites
site <- site_list[indx] # set site:

# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "tidyverse", "rgdal", "devtools")
cluster_dev_packages <- c("dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)
install_clpkg <- lapply(cluster_dev_packages, library, character.only = TRUE)

# load functions
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

print(paste0("Processing raw data.table for ", site, ", run #", run))

# run function

p_dat_derived
site <- "shaanxi"
cc_filter_abn_dt(site = site,
                 path = p_dat_derived,
                 label = "blip1",
                 clean_blips = TRUE)



