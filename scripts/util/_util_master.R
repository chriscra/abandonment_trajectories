# ---------------------------------------------------------------
#
# Master Util script: load all functions in other util scripts
# 
# ---------------------------------------------------------------

p_proj <- "/Users/christophercrawford/Google Drive/_Projects/abandonment_trajectories/"

# load other util scripts ----
source(paste0(p_proj, "scripts/util/_util_misc_functions.R"))
source(paste0(p_proj, "scripts/util/_util_pathnames.R"))
source(paste0(p_proj, "scripts/util/_util_dt_filter_functions.R"))
source(paste0(p_proj, "scripts/util/_util_spatial_functions.R"))