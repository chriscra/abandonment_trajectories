# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 6th, 2020

# Script to convert and save large max_age data.tables as rasters
# -------------------------------------------------------- #
# Note: as of February 2nd, 2021, this has yet to be run. I updated the input path, so should work with how things are structured. 

# array set up
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

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

blip_label <- "_b1"
label <- NULL # for calculating the max age

cat("Set up site parameters:\n")
print(t(site_df[indx, ]))


# load libraries
cluster_packages <- c("data.table", "tictoc")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths
# file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input <- "/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters"


tic.clearlog()
tic("full script")
# load the data
# -------------------------------------------------------- #

# note, look at cc_save_dt_as_raster() function, it does basically the same thing as below

tic("load data")
dt <- fread(file = paste0(p_input, site, "_max_age.csv"))
toc()

# convert age dt to raster
tic("convert dt to raster")
r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc()

# write raster
tic("write raster")
writeRaster(r, filename = paste0(p_input, site, "_max_age", blip_label, ".tif"))
toc()

names(r)
print(r)

toc() # final toc
