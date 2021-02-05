# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 6th, 2020

# Script to convert and save large max_age data.tables as rasters
# -------------------------------------------------------- #
# Note: as of February 2nd, 2021, this has yet to be run. I updated the input path, so should work with how things are structured. 

# load libraries
cluster_packages <- c("data.table", "tictoc")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths
# file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input <- "/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters"

# set site:
site <- "belarus"


tic.clearlog()
tic("full script")
# load the data
# -------------------------------------------------------- #
tic("load data")
dt <- fread(file = paste0(p_input, site, "_max_length.csv"))
toc()

# convert age dt to raster
tic("convert dt to raster")
r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc()

# write raster
tic("write raster")
writeRaster(r, filename = paste0(p_input, site, "_max_length.tif"))
toc()
names(r)
print(r)

toc() # final toc
