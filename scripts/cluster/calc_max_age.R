# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, August 28th, 2020

# Calculate maximum age for each pixel 
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "raster", "rgdal", "sp", "tictoc", "devtools", "dtraster", "parallel")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths
# file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"

name <- "shaanxi"

tic.clearlog()
tic("full script")
# load the data
# -------------------------------------------------------- #
tic("load data")
dt <- fread(file = paste0(p_dat_derived, name, "_age.csv"))
toc(log = TRUE)

# calculate maximum age, serial
tic()
dt[, max_length := max(.SD), .SDcols = -c("x", "y"), by = .(x, y)]
toc(log = TRUE)

# write out just the max_length dt.
tic("write out max age dt")
fwrite(dt[, .(x, y, max_length)], file = paste0(p_dat_derived, name, "_max_length.csv"))
toc(log = TRUE)


toc(log = TRUE) # final toc



# -------------------------------------------- #
# # testing out parallel version -- didn't work
# dt1 <- dt[1:100000]
# dt2 <- dt[1:100000]
# 
# # calculate maximum age, serial
# tic()
# dt2[, max_length := max(.SD), .SDcols = -c("x", "y"), by = .(x, y)]
# toc(log = TRUE) # 1.89
# 
# # calculate maximum age, parallel
# tic("run in parallel")
# dt2 <- mclapply(seq(nrow(dt2)), function(i) {
#   dt2[i][, max_length := max(.SD), .SDcols = -c("x", "y"), by = .(x, y)]
# }, mc.cores = 7)
# toc(log = TRUE)
# 
# 
# tic("recombine")
# dt2 <- do.call("rbind", dt2)
# toc(log = TRUE)
# 
# identical(dt1, dt2)