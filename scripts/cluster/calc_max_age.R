# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 28th, 2020

# Calculate maximum age for each pixel 
# -------------------------------------------------------- #
# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

site_list <- c("shaanxi", "belarus") # list of all sites
site <- site_list[indx] # set site:

blip_label <- "_blip1"
label <- NULL

# load libraries
cluster_packages <- c("data.table", "raster", "rgdal", "sp", "tictoc", "devtools", "dtraster", "tidyverse")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)


# load functions
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"


tic.clearlog()
tic("full script")
# -------------------------------------------------------- #

# calculate maximum age, serial
cc_calc_max_age(directory = p_dat_derived, 
                site = site, blip_label = blip_label,
                label = label) 


toc(log = TRUE) # final toc

