# ---------------------------------------------------------------
#
# Master Util script: load all functions in other util scripts
# 
# ---------------------------------------------------------------
run_label <- 
  "_2022_02_07"
  # "_2022_01_31" 
  # "_2021_03_13"

p_proj <- "/Users/christophercrawford/work/projects/abandonment_trajectories/"

# load other util scripts ----
source(paste0(p_proj, "scripts/util/_util_misc.R"))
source(paste0(p_proj, "scripts/util/_util_pathnames.R"))
source(paste0(p_proj, "scripts/util/_util_functions.R"))


# ----- load files ----
# source(paste0(p_proj, "scripts/util/_util_files.R"))