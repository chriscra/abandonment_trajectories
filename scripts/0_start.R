# ---------------------------------------------------------------
#
# Start File: load packages, functions, and pathnames
# 
# ---------------------------------------------------------------

source("scripts/util/_util_misc_functions.R")
source("scripts/util/_util_pathnames.R")


# -------------------------
# list the needed packages and load the libraries
# -------------------------

needed_packages <- c("raster", "rgdal", "sp", "sf", "mapview", "gdalUtils", "gdata", "GISTools",
          "rgeos", "rasterVis", "RColorBrewer", "ggplot2", "tidyr", "tidyverse",
          "tictoc", "fasterize", "viridis", "lwgeom", "stringr", "magrittr", "pryr",
          "maps", "forcats", "rangeBuilder", "rnaturalearth", "spatialEco", "smoothr", "parallel",
          "ggnewscale", "reshape2", "data.table", "knitr", "rredlist",
          "dtraster")


install_missing_packages(needed_packages)

#install.packages(packages) # old method
# install <- lapply(needed_packages, library, character.only = TRUE) # load them
#update.packages()



