# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, August 28th, 2020

# Script to investigate large belarus data.tables, and primarily 
# 1) save the belarus age data.table as a raster, and then
# 2) calculate max age
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths
# file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

tic.clearlog()
tic("full script")
# load the data
# -------------------------------------------------------- #
tic("load data")
dt <- fread(file = paste0(p_dat_derived, "belarus_age.csv"))
toc()

# convert age dt to raster
tic("convert dt to raster")
r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc()

# write raster
tic("write raster")
writeRaster(r, filename = paste0(p_dat_derived, "belarus_age.tif"))
toc()
names(r)
print(r)

toc() # final toc
