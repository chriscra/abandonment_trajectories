# --------------------------------------------------------------- #
#
# Cluster Start File: load packages, functions, and pathnames
# 
# --------------------------------------------------------------- #

source("util/_util_misc_functions.R")
source("util/_util_functions.R")

# ------------------------- #
# list the needed packages and load the libraries
# ------------------------- #

# needed_packages <- c(
#   "data.table", "raster", "rgdal", "sp", "sf", "mapview", 
#   "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
#   
#   "tidyverse", "lobstr", "pryr", "reshape2",
#   # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
#   
#   # visualization:
#   "rasterVis", "RColorBrewer", "viridis", "scales",
#   "patchwork", "cowplot", # for combining multiple plots
#   "animation", "magick",
#   
#   # development
#   "tictoc",  "magrittr", "parallel", "knitr", 
#   "devtools", "Hmisc", "acepack",
#   
#   # spatial, ecological packages, extra:
#   "rnaturalearth", "smoothr", "spatialEco", "maps", "rangeBuilder",
#   "rredlist",
#   "landscapemetrics", "landscapetools"
#   )


cluster_packages <- c(
  "data.table", "raster", "terra", "rgdal", "sp", "sf",
  "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
  "tidyverse", "lobstr", "pryr", "reshape2",  # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
  "tictoc",  "magrittr", "parallel",
  "devtools", "Hmisc",
  "landscapemetrics", "landscapetools"
)

# nice to have, but not needed:
# c("ggnewscale", )

# ------------------------- #
# function to install only missing packages
# ------------------------- #
install_missing_packages <- function(list_of_packages) {
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repo = 'https://cloud.r-project.org/')
  } 
  sapply(list_of_packages, library, character.only = TRUE)
}

# install_missing_packages(needed_packages)
install_missing_packages(cluster_packages)


#install.packages(needed_packages) # old method
# install <- lapply(needed_packages, library, character.only = TRUE) # load them
#update.packages()



# ------------------------- #
# Additional development packages, to be installed with devtools:
# ------------------------- #

# ---- #
# Lyndon's packages

# devtools::install_github("ldemaz/dtraster")
# devtools::install_github("PrincetonUniversity/lmisc")
# devtools::install_github("PrincetonUniversity/agroEcoTradeoff@devel")

# ---- #
# rnaturalearth extras

# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")

# ---- #
# others
# devtools::install_github('BigelowLab/dismotools')

# ---- #
# load
github_packages <- c(
  #"agroEcoTradeoff",
  #"rnaturalearthdata", "rnaturalearthhires",
  #"dismotools",
  "dtraster", "lmisc")

github_packages_inst <- sapply(github_packages, library, character.only = TRUE) # load them



# ------------------------- #
# pathnames:
# ------------------------- #
p_dat <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

