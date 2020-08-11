# ---------------------------------------------------------------
#
# Install and Load Base R Packages
# 
# ---------------------------------------------------------------

# good for fresh installations of R

# -------------------------
# Create function to install only missing packages
# -------------------------
install_missing_packages <- function(list_of_packages) {
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repo = 'https://cloud.r-project.org/')
  } 
  sapply(list_of_packages, require, character.only = TRUE)
}


# -------------------------
# list the needed packages and load the libraries
# -------------------------

needed_packages <- c("raster", "rgdal", "sp", "sf", "mapview", "gdalUtils", "gdata", "GISTools",
                     "rgeos", "rasterVis", "RColorBrewer", "ggplot2", "tidyr", "tidyverse",
                     "tictoc", "fasterize", "viridis", "lwgeom", "stringr", "magrittr", "pryr",
                     "maps", "forcats", "rangeBuilder", "rnaturalearth", "spatialEco", "smoothr", "parallel",
                     "ggnewscale", "reshape2", "data.table", "knitr", "rredlist",
                     "dtraster",
                     "scales",
                     "patchwork",
                     "devtools",
                     "Hmisc", "acepack")


install_missing_packages(needed_packages)


#install.packages(packages) # old method
# install <- lapply(needed_packages, library, character.only = TRUE) # load them
#update.packages()

# -------------------------
# Lyndon's packages, which need to be installed with devtools:
# -------------------------

# install using devtools
devtools::install_github("ldemaz/dtraster")
devtools::install_github("PrincetonUniversity/lmisc")
devtools::install_github("PrincetonUniversity/agroEcoTradeoff@devel")


# load
lyndon_pkgs <- c("dtraster", "lmisc", "agroEcoTradeoff")
lyndon_inst <- lapply(lyndon_pkgs, library, character.only = TRUE) # load them



