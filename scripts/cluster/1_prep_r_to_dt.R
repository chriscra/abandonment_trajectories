# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 4th, 2021 (updated January 28th, 2022)

# Script to prep land cover rasters for all sites, 
# producing data.tables for further analysis.
# -------------------------------------------------------- #
# Note: this script merges, recodes, and preps land cover rasters from Yin et al. 2020,
# saving them for use in "2_analyze_abn.R", "2.2_calc_recult_age.R", and "3_distill_lengths.R"

# It contains the following steps:
# 1. cc_merge_rasters() // Merge raw raster layers
# 2. cc_r_to_dt() // Convert raw rasters into data.tables (including renaming and recoding)
# 3. Pass temporal filter across raw land cover data.tables
# 4. Save prepped land cover data.tables as rasters.

# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", 
                      "raster", "terra", 
                      "landscapemetrics", "landscapetools", "sp",
                      "rgdal", "sf", "fasterize",
                      "dtraster",
                      "tidyverse"
                      )
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
# site_index <- as.numeric(args[1])
# indx <- 9


tic.clearlog()
tic("full script")

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label


cat("Set up site parameters:", fill = TRUE)
print(t(site_df[indx, ]))


# -------------------------------------------------------- #
# 1. Merge raw raster layers ----
cat("1. Merge raw raster layers", fill = TRUE)
tic("Merge rasters")
cc_merge_rasters_terra(site = site, 
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
# 3. Pass temporal filter across land cover maps ----
# Adapted from "temporal_filter_lc.R"

# load the data
tic("load lc data.table")
lc_dt <- fread(file = paste0(p_input_rasters, site, ".csv"))
toc(log = TRUE)

print(paste0("ncell, before filter: ", nrow(lc_dt)))

# pass temporal filter
tic("pass temporal filter")
cc_temporal_filter_lc(lc_dt)
toc()

print(paste0("ncell, after filter: ", nrow(lc_dt)))

# write to file
fwrite(lc_dt, file = paste0(p_input_rasters, site, "_clean.csv"))

cat(paste0("Success! Passed temporal filter on land cover data.table for site: ", site_df$site[indx]), fill = TRUE)


# -------------------------------------------------------- #
# 4. Save renamed and recoded input land cover data.tables (.csvs) as rasters ----
# to serve as canonical inputs going forward.
# This is adapted from "dt_age_to_raster.R"

# convert dts to raster
# read back in the un-filtered version:
lc_dt_unfiltered <- fread(file = paste0(p_input_rasters, site, ".csv"))

tic("convert un-filtered dt to raster")
r_unfiltered <- dt_to_raster(lc_dt_unfiltered, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc(log = TRUE)

tic("convert cleaned dt to raster")
r <- dt_to_raster(lc_dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc(log = TRUE)

names(r)
print(r)

# write raster
tic("write un-filtered raster")
raster::writeRaster(r_unfiltered, 
                    filename = paste0(p_input_rasters, site, "_tmp.tif"),
                    overwrite = TRUE)
toc(log = TRUE)

tic("write cleaned raster")
raster::writeRaster(r, 
                    filename = paste0(p_input_rasters, site, "_clean_tmp.tif"),
                    overwrite = TRUE)
toc(log = TRUE)

# reload raster as SpatRaster
r_unfiltered <- terra::rast(paste0(p_input_rasters, site, "_tmp.tif"))
r <- terra::rast(paste0(p_input_rasters, site, "_clean_tmp.tif"))

# extract names:
# rename raster layers:
if (site == "nebraska") {
  layer_names <- paste0("y", 1986:2018)
} else {
  if (site == "wisconsin") {
    layer_names <- paste0("y", 1987:2018)
  } else {
    # everything else, just 1987:2017
    layer_names <- paste0("y", 1987:2017)
  }}

# write new cleaned SpatRasters to file, with layer names
tic("write new cleaned lc raster to file")
terra::writeRaster(r,
                   filename = paste0(p_input_rasters, site, "_clean.tif"),
                   overwrite = TRUE,
                   names = layer_names)
toc()

tic("write unfiltered lc raster to file")
terra::writeRaster(r_unfiltered,
                   filename = paste0(p_input_rasters, site, ".tif"),
                   overwrite = TRUE,
                   names = layer_names)
toc()

# -------------------------------------------------------- #

toc(log = TRUE) # final toc
tic.log(format = TRUE) # print the tic log

