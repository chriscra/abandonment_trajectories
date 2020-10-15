# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 14th, 2020

# Script to generate summary data.frames for plotting.
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# source functions:
source("/home/clc6//abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")

# ------------------------------------------ #
# ----------------- Belarus ---------------- #
# ------------------------------------------ #
# load data:
b <- brick(paste0(p_dat_derived, "belarus.tif")) # merged version
names(b) <- paste0("y", 1987:2017)
b_age <- fread(input = paste0(p_dat_derived, "belarus_age.csv"))
b_dt <- fread(input = paste0(p_dat_derived, "belarus.csv")) 


tic()
cc_generate_dfs(
  land_cover_dt = b_dt, 
  abn_age_dt = b_age, 
  land_cover_raster = b$y2017,
  outfile_label = "_b",
  abandonment_threshold = 5,
  include_all = TRUE
)
toc()
