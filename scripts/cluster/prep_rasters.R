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
indx <- as.numeric(args[1]) # 1:9, or whatever the array variable is 

site_list <- c("bosnia_herzegovina", "chongqing", "goias", 
               "iraq", "mato_grosso", "nebraska",
               "orenburg", "volgograd", "wisconsin") # list of all sites


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
raw_dir_path <- "/scratch/network/clc6/abandonment_trajectories/final_maps_raw/"


# load site_df
site_df <- read_csv(file = paste0(p_dat_derived, "site_df.csv"))



print(paste0("Processing raw raster for ", site))

# run function

tic("merge rasters")
cc_merge_rasters(site = site, site_df = site_df, input_path = raw_dir_path)
toc(log = TRUE)



tic("recode rasters")
cc_recode_rasters(site = site, site_df = site_df, input_path = raw_dir_path, 
                  output_path = paste0(p_dat_derived, "input_rasters/"))
toc(log = TRUE)





