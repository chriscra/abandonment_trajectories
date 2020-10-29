# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 27th, 2020

# Script to summarize abandonment data.tables into data.frames
# to use when plotting.
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


# load data:

# make this run for a given site.
# then make this runnable by array
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

