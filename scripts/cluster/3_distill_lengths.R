# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 9th, 2021 (updated March 13th, 2021, January 28th, 2022)

# Script to synthesize the abandonment length data resulting from
# "2_analyze_abn.R" and "2.2_calc_recult_age.R" scripts, calculating abandonment and recultivation lengths for all sites
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
p_tmp           <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/tmp/"


# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# set run_label
run_label <- format(Sys.time(), "_%Y_%m_%d") 
# run_label <- "_2022_01_31" #"_2021_03_13" # "_2021-03-05"

cat(fill = TRUE, "Distilling length data.tables for run:", run_label)


# -------------------------------------------------------- #
# Distill the abandonment length data, for use in histograms ----

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

# save length_distill_df
write_csv(length_distill_df, file = paste0(p_input_rasters, "length_distill_df", run_label, ".csv"))

cat(fill = TRUE, "Saved length_distill_df to:", paste0(p_input_rasters, "length_distill_df", run_label, ".csv"))



# -------------------------------------------------------- #
# Distill the recultivation length data, for use in histograms ----

recult_length_distill_df <- lapply(site_df$site, function(x){
  recult_length <- fread(input = paste0(p_input_rasters, x, "_recult_length", run_label, ".csv"))
  
  
  dt <- recult_length[, .(freq = .N), by = length
                      ][, ':='(site = x, length_type = "recult")]
  
  cat(fill = TRUE, "distilled:", x)
  
  # return dt
  dt
}
) 

recult_length_distill_df <- recult_length_distill_df %>% bind_rows() %>% as.data.frame()

# save recult_length_distill_df
write_csv(recult_length_distill_df, file = paste0(p_input_rasters, "recult_length_distill_df", run_label, ".csv"))

cat(fill = TRUE, "Saved recult_length_distill_df to:", paste0(p_input_rasters, "recult_length_distill_df", run_label, ".csv"))



# -------------------------------------------------------- #
# Distill the *potential* length data, for use in histograms ----

potential_length_distill_df <- lapply(site_df$site, function(x){

  potential_length <- fread(input = paste0(p_input_rasters, x, "_potential_length", run_label, ".csv"))
  
  
  dt <- potential_length[, .(freq = .N), by = length
                      ][, ':='(site = x, length_type = "potential")]
  
  cat(fill = TRUE, "distilled:", x)
  
  # return dt
  dt
}
) 

potential_length_distill_df <- potential_length_distill_df %>% bind_rows() %>% as.data.frame()

# save potential_length_distill_df
write_csv(potential_length_distill_df, file = paste0(p_input_rasters, "potential_length_distill_df", run_label, ".csv"))

cat(fill = TRUE, "Saved potential_length_distill_df to:", paste0(p_input_rasters, "potential_length_distill_df", run_label, ".csv"))




