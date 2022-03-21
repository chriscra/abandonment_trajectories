# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, July 8th, 2021

# Script to *ONLY* summarize fully processed data.tables, 
# breaking out step 4 of "2_analyze_abn.R" to be stand-alone script.
# -------------------------------------------------------- #

# Originally written to implement updated area estimates from terra::cellSize()

# Steps include:
# 4. cc_summarize_abn_dts() // Summarize the abandonment datatables into dataframes for plotting purposes.

# SLURM Scripts that accompany this are:
# 2.1_summarize_abn_dts_only.slurm

# ------------ runs ------------ #
# 0th run (_b1) - only blip filter (just 101)
# 1st run with new temporal filters, and also filtering the edge (_2021-03-05)
# 2nd run, with temporal filters but NOT filtering the edge (_2021_03_13) (filter_edge = FALSE)
# (*LATEST*) 3rd run, just to summarize the data.tables again (step 4 above), implementing the new area calculation from the terra package.

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


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/_util/_util_functions.R")


# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label

# specify run
run_label <- "_2022_02_07"


cat(fill = TRUE, "Set up site parameters:")
print(t(site_df[indx, ]))

cat(fill = TRUE, "Summarizing data.tables for the following run: ", run_label)


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



