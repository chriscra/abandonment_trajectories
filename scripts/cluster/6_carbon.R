# Calculate carbon accumulation in 

# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived   <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/gpfs/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/gpfs/clc6/abandonment_trajectories/raw_rasters/"
p_tmp           <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/tmp/"


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")


# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# time stamp
run_label <- "_2022_02_07"

abandonment_threshold <- 5

# 0. Load rasters
# load combined carbon accumulation rasters
carbon_t <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "carbon/", site_df$site[i], "_forest_soc_combined_per_pixel.tif"))
})
for (i in seq_along(carbon_t)) {names(carbon_t[[i]]) <- c("Y20", "YSS")}

# load max_age rasters
max_age_t <- lapply(1:11, function(i) {
  terra::rast(paste0(p_input_rasters, site_df$site[i], "_max_age", run_label, ".tif"))
})
for (i in seq_along(max_age_t)) {names(max_age_t[[i]]) <- "max_age"}


# function:
# testing
# site_indx <- 1
# input_type <- "_age"

# start lapply() loop:
carbon_df <- lapply(1:11, function(i) {
  carbon_df <- 
    cc_calc_carbon_accumulation(input_type = "_age", 
                                   site_indx = i,
                                   abandonment_threshold = 5)

  potential_carbon_df <- 
    cc_calc_carbon_accumulation(
      input_type = "_potential_age", 
      site_indx = i,
      abandonment_threshold = 5)
  
  carbon_df <- 
    rbind(carbon_df %>% mutate(type = "Observed"),
          potential_carbon_df %>% mutate(type = "Potential"))
  
  # save to file
  write_csv(carbon_df, paste0(p_dat_derived, "carbon/", site_df$site[i],
                              "_carbon_df", run_label, ".csv"))
  
  }) %>% bind_rows()

# write out combined file
write_csv(carbon_df, paste0(p_dat_derived, "carbon/", "carbon_df", run_label, ".csv"))
