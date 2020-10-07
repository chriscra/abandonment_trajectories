# ---------------------------------------------------------------
#
# Start File: load packages, functions, and pathnames
# 
# ---------------------------------------------------------------

source("scripts/util/_util_misc_functions.R")
source("scripts/util/_util_pathnames.R")
source("scripts/util/_util_spatial_functions.R")
source("scripts/util/_util_dt_filter_functions.R")



# -------------------------
# list the needed packages and load the libraries
# -------------------------

needed_packages <- c(
  "data.table", "raster", "rgdal", "sp", "sf", "mapview", 
  "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
  
  "tidyverse", "lobstr", "pryr", "reshape2",
  # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
  
  # visualization:
  "rasterVis", "RColorBrewer", "viridis", "scales",
  "patchwork", "cowplot", # for combining multiple plots
  "animation", "magick",
  
  # development
  "tictoc",  "magrittr", "parallel", "knitr", 
  "devtools", "Hmisc", "acepack",
  
  # spatial, ecological packages, extra:
  "rnaturalearth", "smoothr", "spatialEco", "maps", "rangeBuilder",
  "rredlist",
  "landscapemetrics", "landscapetools"
  )


# cluster_packages <- c(
#   "data.table", "raster", "rgdal", "sp", "sf",
#   "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
#   "tidyverse", "lobstr", "pryr", "reshape2",  # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
#   "tictoc",  "magrittr", "parallel",
#   "devtools", "Hmisc"
# )

# nice to have, but not needed:
# c("ggnewscale",)

install_missing_packages(needed_packages)

#install.packages(needed_packages) # old method
# install <- lapply(needed_packages, library, character.only = TRUE) # load them
#update.packages()



# -------------------------
# Additional development packages, to be installed with devtools:
# -------------------------

# ----
# Lyndon's packages

# devtools::install_github("ldemaz/dtraster")
# devtools::install_github("PrincetonUniversity/lmisc")
# devtools::install_github("PrincetonUniversity/agroEcoTradeoff@devel")

# ----
# rnaturalearth extras

# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")

# ----
# others
# devtools::install_github('BigelowLab/dismotools')

# ----
# load
github_packages <- c(
  "dtraster", "lmisc", "agroEcoTradeoff",
  "rnaturalearthdata", "rnaturalearthhires",
  "dismotools")

github_packages_inst <- sapply(github_packages, library, character.only = TRUE) # load them

