# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, November 6th, 2020

# Script to process rasters for all sites, doing the following:
# 1. 
# -------------------------------------------------------- #

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

site_list <- c("shaanxi", "belarus") # list of all sites
site_label_list <- c("_s", "_b")

site <- site_list[indx] # set site:
site_label <- site_label_list[indx] # set label

blip_label <- "_blip1"

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# source functions:
source("/home/clc6//abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")



# 1. Convert raw rasters into data.tables
print(paste0("Converting raw rasters to data.tables: ", site))
cc_r_to_dt(site = site, path = p_dat_derived)

# 2. Process raw data.tables in order to calculate the length of agricultural abandonment periods.
      # Steps include:
      # 1) filtering to just abandonment cells, 
      # 2) filling recultivation blips based on a threshold,
      # 3) calculating age, and 4) extracting lengths, all the while writing out files.

print(paste0("Filter data.tables for abandonment raw rasters to data.tables: ", site))

cc_filter_abn_dt(site = site,
                 path = p_dat_derived,
                 label = blip_label,
                 clean_blips = TRUE)

# 3. 

# 4. 
lc_raster <- brick(paste0(p_dat_derived, site, ".tif")) # merged version
names(lc_raster) <- paste0("y", 1987:2017)

lc_dt <- fread(input = paste0(p_dat_derived, site, ".csv")) 

age_dt <- fread(input = paste0(p_dat_derived, site, "_age", blip_label, ".csv"))

print(paste0("Summarizing abandonment data.tables for: ", site))
tic()
cc_summarize_abn_dts(
  land_cover_dt = lc_dt, 
  abn_age_dt = age_dt, 
  land_cover_raster = lc_raster$y2017,
  outfile_label = paste0(blip_label, site_label),
  abandonment_threshold = 5,
  include_all = TRUE
)
toc()

