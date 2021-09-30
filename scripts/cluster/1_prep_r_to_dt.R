# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 4th, 2021

# Script to prep land cover rasters for all sites, 
# producing data.tables for further analysis.
# -------------------------------------------------------- #
# Note: this script merges, recodes, and preps land cover rasters from Yin et al. 2020,
# saving them for use in "2_analyze_abn.R"

# It contains three steps:
# 1. cc_merge_rasters() // Merge raw raster layers
# 2. cc_r_to_dt() // Convert raw rasters into data.tables (including renaming and recoding)
# 3. save prepped land cover data.tables as rasters.

# SLURM Scripts that accompany this are:
# analyze_all_sites.slurm, which runs for all sites except belarus (1), which requires a greater memory allocation.
# orenburg was close, but I was able to make it work with 150 gb. total.


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
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")


# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])


tic.clearlog()
tic("full script")

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# site_list <- c("belarus", "bosnia_herzegovina", "chongqing", 
#                "goias", "iraq", "mato_grosso", 
#                "nebraska", "orenburg", "shaanxi", 
#                "volgograd", "wisconsin")
# 
# site_label_list <- c("_b", "_bh", "_c", 
#                      "_g", "_i", "_mg", 
#                      "_n", "_o", "_s", 
#                      "_v", "_w")

site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label


cat("Set up site parameters:", fill = TRUE)
print(t(site_df[indx, ]))


# -------------------------------------------------------- #
# 1. Merge raw raster layers ----
cat("1. Merge raw raster layers", fill = TRUE)
tic("Merge rasters")
cc_merge_rasters(site = site, 
                 site_df = site_df, 
                 input_path = p_raw_rasters
                 # ^^ this folder must contain folders with site names, 
                 # with either bricks or multiple raster layer files inside
                 )
toc(log = TRUE)


# -------------------------------------------------------- #
# 2. Convert raw rasters into data.tables (including renaming and recoding) ----
tic("convert raw rasters into data.tables, including renaming and recoding")
cat("2. Converting raw rasters into data.tables (including renaming and recoding): ", site, fill = TRUE)
cc_r_to_dt(site = site, site_df = site_df,
           input_path = p_raw_rasters, # with the raw, merged raster files directly in the directory
           output_path = p_input_rasters # where the recoded .csv and .tif files will go
           )
toc(log = TRUE)


# -------------------------------------------------------- #
# 3. (Recently added). Save renamed and recoded input land cover data.tables (.csvs) as rasters, 
# to serve as canonical inputs going forward.
# This is adapted from dt_age_to_raster.R

# load the data
tic("load data")
dt <- fread(file = paste0(p_input_rasters, site, ".csv"))
toc(log = TRUE)

# convert age dt to raster
tic("convert dt to raster")
r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc(log = TRUE)

names(r)
print(r)


# write raster
tic("write raster")
writeRaster(r, filename = paste0(p_input_rasters, site, ".tif"))
toc(log = TRUE)


# -------------------------------------------------------- #

toc(log = TRUE) # final toc
tic.log(format = TRUE) # print the tic log

