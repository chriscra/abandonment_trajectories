# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 9th, 2021 (updated March 13th, 2021)

# Script to synthesize the abandonment length data resulting from
# "2_analyze_abn.R" script, calculating abandonment trajectories for all sites
# -------------------------------------------------------- #


# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster", "pryr")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived   <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/gpfs/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/gpfs/clc6/abandonment_trajectories/raw_rasters/"


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# set run_label
run_label <- "_2021_03_13" # "_2021-03-05"

cat(fill = TRUE, "Distilling length data.tables for run:", run_label)


# -------------------------------------------------------- #
# Distill the length data, for use in histograms ----

length_distill_df <- lapply(site_df$site, function(x){
  length <- fread(input = paste0(p_input_rasters, x, "_length", run_label, ".csv"))
  max_age <- fread(input = paste0(p_input_rasters, x, "_max_age", run_label, ".csv"), 
                   select = "max_age") # drop x and y, makes loading and calculating much faster
  
  dt <- length[, .(freq = .N), by = length
               ][, ':='(site = x, length_type = "all")]
  
  dt_max <- max_age[, .(freq = .N), by = max_age
                    ][order(max_age), 
                      ][, ':='(site = x, length_type = "max")
                        ][, length := max_age
                          ][, max_age := NULL][]
  
  dt <- bind_rows(dt, dt_max) 
  cat(fill = TRUE, "distilled:", x)
  
  # return dt
  dt
}
) 

length_distill_df <- length_distill_df %>% bind_rows() %>% as.data.frame()

# save site_df
write_csv(length_distill_df, file = paste0(p_input_rasters, "length_distill_df", run_label, ".csv"))

cat(fill = TRUE, "Saved length_distill_df to:", paste0(p_input_rasters, "length_distill_df", run_label, ".csv"))





