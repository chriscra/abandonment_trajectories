# ---------------------------------------------------------------
#
# Miscellaneous functions
# 
# ---------------------------------------------------------------


# -------------------------
# Install only missing packages
# -------------------------
install_missing_packages <- function(list_of_packages) {
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repo = 'https://cloud.r-project.org/')
  } 
  sapply(list_of_packages, require, character.only = TRUE)
}


# -------------------------
# Update packages
# -------------------------

#update.packages()


# -------------------------
# calculate the sizes of items in the environment
# -------------------------
env_size <- function(workspace = ls()) {
  size = 0
  for (x in workspace){
    thisSize = object_size(get(x))
    size = size + thisSize
    message(x, " = ", appendLF = F); print(thisSize, units='auto')
  }
  message("total workspace is ", appendLF = F); print(size, units='auto')
}