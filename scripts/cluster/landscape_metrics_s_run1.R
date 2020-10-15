# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 12th, 2020

# Script to calculate various landscape metrics, 
# using the R package landscapemetrics
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# set site:
site <- "shaanxi"
run <- 1
print("Calculating landscapemetrics for: ")
print(site)


tic.clearlog()
tic("Full script")
# load the data
# -------------------------------------------------------- #

# (re)load reprojected rasters
tic("load the cleaned reprojected rasters")
reprojected_r <- brick(paste0(p_dat_derived, site, "_reproj_0rm.tif"))
toc(log = TRUE)
names(reprojected_r) <- paste0("y", 1987:2017) # rename

# check_landscapes
tic("check landscape")
check_reprojected_r <- check_landscape(reprojected_r)
print(check_reprojected_r)
toc(log = TRUE)

# select landscape metrics to run:
# what matters to species? size of patches, shape/area edge perimeter, connectivity
list_lsm() %>% arrange(level, type, name) %>% select(function_name, name, type, level) %>% print(n = 132)

metrics_list_c <- c(
  # by class 
  # aggregation
  "lsm_c_ai", # aggregation index, class level (RS has used this one)
  "lsm_c_clumpy", # clumpiness index, class (maybe)
  "lsm_c_mesh", # effective mesh size, class
  
  "lsm_c_enn_cv", # euclidean nearest neighbor distance, cv, class
  "lsm_c_enn_mn", # euclidean nearest neighbor distance, mean, class
  "lsm_c_enn_sd", # euclidean nearest neighbor distance, sd, class
  
  "lsm_c_np", # number of patches, class
  "lsm_c_cohesion", # patch cohesion index, class (RS has used this one)
  "lsm_c_pd", # patch density (maybe)
  
  # area and edge metrics
  "lsm_c_area_cv", # patch area, cv, per class
  "lsm_c_area_mn", # patch area, mean, per class
  "lsm_c_area_sd", # patch area, sd, per class
  
  "lsm_c_ca", # total (class) area
  "lsm_c_lpi", # largest patch index, class
  
  # core area metrics
  "lsm_c_cai_cv", # core area index, cv, class
  "lsm_c_cai_mn", # core area index, mn, class
  "lsm_c_cai_sd", # core area index, sd, class
  

  # shape metric
  "lsm_c_contig_cv", # contiguity index, class, cv
  "lsm_c_contig_mn", # contiguity index, class, mean
  "lsm_c_contig_sd", # contiguity index, class, sd
  
  "lsm_c_pafrac", # perimeter-area fractal dimension
  
  "lsm_c_para_cv", # perimeter-area ratio, cv
  "lsm_c_para_mn", # perimeter-area ratio, mean
  "lsm_c_para_sd", # perimeter-area ratio, sd
  
  "lsm_c_shape_cv", # shape index, cv
  "lsm_c_shape_mn", # shape index, mean
  "lsm_c_shape_sd" # shape index, sd
)

metrics_list_l <- c(
  # landscape
  # aggregation metrics
  "lsm_l_ai", # aggregation index, landscape level
  "lsm_l_contag", # connectance
  "lsm_l_mesh", # effective mesh size, landscape

  "lsm_l_contig_cv", # contiguity index, landscape, cv
  "lsm_l_contig_mn", # contiguity index, landscape, mean
  "lsm_l_contig_sd", # contiguity index, landscape, sd

  "lsm_l_cohesion", # patch cohesion index, landscape
  "lsm_l_np", # number of patches
  "lsm_l_pd", # patch density
  # shape metric
  # area and edge metrics
  "lsm_l_area_mn", # patch area, mean, landscape overall
  "lsm_l_lpi" # largest patch index, landscape
)

# list the metrics
print(list_lsm(what = metrics_list_c))


tic("calculate lsm")
frag_metrics <- reprojected_r %>%
  calculate_lsm(
    what = metrics_list_c[c(1:5)],
    classes_max = 5,
    verbose = TRUE, progress = TRUE
  ) %>%
  dplyr::left_join(x = .,
                   y = lsm_abbreviations_names, 
                   by = c("metric", "level"))
toc(log = TRUE)

# Save file
tic()
write_rds(frag_metrics, paste0(p_output, "frag_metrics_", site, run, ".rds"))
toc(log = TRUE)


toc(log = TRUE) # final toc

print(tic.log())
