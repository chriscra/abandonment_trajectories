# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 4th, 2021 
# (updated March 13th, 2021, January 28th, 2022)

# Script to analyze abandonment trajectories rasters for all sites
# -------------------------------------------------------- #
# Note: this is the primary script to analyze abandonment trajectories in the
# land cover rasters from Yin et al. 2020. It contains 4 steps, starting 
# with the prepped data.tables resulting from "1_prep_r_to_dt.R" and producing 
# various results data.tables, which are further synthesized into results data.frames.

# Steps include:
# 1. cc_filter_abn_dt() // Process raw data.tables in order to calculate the length of agricultural abandonment periods.
    # Steps include:
    # 1) filtering to just abandonment cells, 
    # 2) filling recultivation blips based on a threshold,
    # 3) calculating age, and 
    # 4) extracting lengths, all the while writing out files.
# 2. cc_calc_max_age() // Calculate maximum age, in serial.
# 3. cc_save_dt_as_raster() // Save various data.tables as rasters.
# 4. cc_summarize_abn_dts() // Summarize the abandonment datatables into dataframes for plotting purposes.

# SLURM Scripts that accompany this are:
# 2_analyze_abn.slurm

# 0th run (_b1) - only blip filter (just 101)
# 1st run with new temporal filters, and also filtering the edge (_2021-03-05)
# 2nd run, with temporal filters but NOT filtering the edge (_2021_03_13) (filter_edge = FALSE)
# 3rd run, after moving the temporal filtering stage to raw land cover dts, 
#   and adding recultivation calculator (_2022_01_31).

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


# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")



# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label

# time stamp
time_stamp <- format(Sys.time(), "_%Y_%m_%d") #paste0("_", Sys.Date()) # format(Sys.time(), "_%Y-%m-%d_%H%M%S")
run_label <- paste0(#site_label, 
                    time_stamp)



cat(fill = TRUE, "Set up site parameters:")
print(t(site_df[indx, ]))

cat(fill = TRUE, "Run label (time stamp):", run_label)



# -------------------------------------------------------- #
# 1. Process raw data.tables in order to calculate the length of agricultural abandonment periods. ----
      # Steps include:
      # 1) filtering to just abandonment cells, 
      # 2) filling recultivation blips based on a threshold,
      # 3) calculating age, and 4) extracting lengths, all the while writing out files.

cat("1. Filter data.tables for abandonment raw rasters to data.tables: ", site, fill = TRUE)


cc_filter_abn_dt(site = site,
                 path = p_input_rasters,
                 run_label = run_label,
                 # note, now temporal filtering step has been moved to land cover map, instead of after recoded map
                 load_precleaned_lc = TRUE,
                 pass_temporal_filter = FALSE, filter_edge = FALSE,
                 temporal_filter_replacement_value = 1)


# -------------------------------------------------------- #
# 2. Calculate maximum age, serial ----
cat("2. Calculate maximum age, in serial:", site, fill = TRUE)
cc_calc_max_age(directory = p_input_rasters,
                site = site,
                run_label = run_label)


# -------------------------------------------------------- #
# 3. save various data.tables as rasters ----
cat("3. Save data.tables as rasters: ", site, fill = TRUE)

dt_ids <- c(
  # these first two are not strictly necessary - see "1_prep_r_to_dt.R"
  # "",  # the re-coded land cover .csv (without the temporal filter)
  # "_clean",  # the cleaned land cover .csv (after applying the temporal filter)
  paste0("_max_age", run_label),  # can also include the max age raster
  paste0("_age", run_label) # the abandonment age .csv
)

cat("types of data.tables to save as rasters:", fill = TRUE)
print(dt_ids)

for (i in dt_ids) {
  cc_save_dt_as_raster(site = site,
                       type = i,
                       input_path = p_input_rasters,
                       output_path = p_input_rasters)
}


# -------------------------------------------------------- #
# 4. Summarize the abandonment data.tables into data.frames for plotting purposes ----
cat("4. Summarizing abandonment data.tables: ", site, fill = TRUE)

cc_summarize_abn_dts(
  input_path = p_input_rasters,
  output_path = p_input_rasters, # could also be p_output, this is just where the .rds goes.
  site = site,
  run_label = run_label,
  abandonment_threshold = 5,
  include_all = TRUE
)



