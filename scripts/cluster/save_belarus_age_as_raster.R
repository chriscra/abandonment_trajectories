# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, August 27th, 2020

# Save belarus data.table as a raster
# -------------------------------------------------------- #

dt <- fread(file = paste0(directory, name, "_age.csv"))

# convert age dt to raster
r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))

# write raster
writeRaster(r, filename = paste0(directory, name, "_age.tif"))

# load libraries
cluster_packages <- c("data.table", "raster", "rgdal", "sp", "tictoc", "devtools")
cluster_dev_packages <- c("dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE) # load them
install_clpkg <- lapply(cluster_dev_packages, library, character.only = TRUE) # load them


# set paths
file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
path_out <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

# load functions
source("/home/clc6/abandonment_trajectories/scripts/util/_util_dt_filter_functions.R")