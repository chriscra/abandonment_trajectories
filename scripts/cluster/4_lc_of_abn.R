# Final land cover classes of abandoned and recultivated lands
# Christopher Crawford, March 23rd, 2021, updated Feb 1, 2022
# ----------------------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra", 
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived   <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/gpfs/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/gpfs/clc6/abandonment_trajectories/raw_rasters/"
p_tmp           <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/tmp/"


# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# source functions:
source("/home/clc6/abandonment_trajectories/scripts/util/_util_functions.R")

# tic.clearlog()
# tic("full script")

# set up parameters:
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# run_label (time stamp)
run_label <- format(Sys.time(), "_%Y_%m_%d") 
# run_label <- "_2022_01_31" #"_2021_03_13" # "_2021-03-05"



# ----------------------------------------------------------------------------- #
# Part I: What proportion of the land that was abandoned at each site falls into the different land cover classes?
# ----------------------------------------------------------------------------- #

# 1. Easiest method is to make a stack out of:
#   a) abandonment age rasters, year 2017, 
#   b) land cover rasters, year 2017, 
#   c) area (ha) raster
# 2. Convert to data.tables
# 3. Calculate summary statistics

# load site input land cover rasters:
# --------------- list of all sites ----------------- #
# prepared input rasters (derived by Chris)

site_t <- lapply(1:11, function(i) {
  terra::rast(paste0(p_input_rasters, site_df$site[i], "_clean.tif"))
})
names(site_t) <- site_df$site

# rename raster layers:
for (i in 1:11) {
  if (names(site_t[i]) == "nebraska") {
    names(site_t[[i]]) <- paste0("y", 1986:2018)
  } else {
    if (names(site_t[i]) == "wisconsin") {
      names(site_t[[i]]) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      names(site_t[[i]]) <- paste0("y", 1987:2017)
    }}}

# ----------------- load abandonment age rasters ---------------- #
# abandonment age maps (produced by Chris)
age_t <- lapply(1:11, function(i) {
  terra::rast(paste0(p_input_rasters, site_df$site[i], "_age", run_label, ".tif"))
})
names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017



# stack and manipulate #

abn_lc_area_2017 <- lapply(site_df$site, function(i) {
  # Select just the years 2017, and convert to data.tables
  r17 <- c(
    site_t[[i]]$y2017, # land cover in 2017 (Raster*)
    age_t[[i]]$y2017, # abandonment age in 2017 (Raster*)
    terra::cellSize(age_t[[i]]$y2017, unit = "ha", mask = FALSE) # area (ha), calculated as SpatRaster,
  )
  names(r17) <- c("lc_2017", "age_2017", "area_ha")
  # plot(r17)

  # convert to data.tables
  dt17 <- spatraster_to_dt(r17)
  
  # where age is greater than or equal to 5, what is the land cover class breakdown?
  # calculate the sum of area, pixels, etc.
  lc_area <- dt17[age_2017 >= 5, 
                   .(pixel_count = .N, sum_area_ha = sum(area_ha)), 
                   by = .(lc_2017, age_2017)] %>% 
    as_tibble() %>%
    arrange(lc_2017, age_2017) %>%
    mutate(site = i)
  
  lc_area
  }
) %>% bind_rows()

# save the lc_area_2017 tibble
write_csv(abn_lc_area_2017, file = paste0(p_dat_derived, "abn_lc_area_2017", run_label, ".csv"))
# abn_lc_area_2017 <- read_csv(file = paste0(p_dat_derived, run_label, "/derived_data/", "abn_lc_area_2017", run_label, ".csv"))


# what proportion of the abandoned land at each site ends up in forest vs. grassland?
abn_prop_lc_2017 <- abn_lc_area_2017 %>% 
  group_by(site, lc_2017) %>%
  summarise(lc_freq = sum(pixel_count),
            lc_area_ha = sum(sum_area_ha)) %>%
  left_join(., 
            abn_lc_area_2017 %>% 
              group_by(site) %>%
              summarise(abn_freq = sum(pixel_count),
                        total_abn_ha = sum(sum_area_ha))) %>%
  mutate(prop_lc_pixels = lc_freq/abn_freq,
         prop_lc_area_ha = lc_area_ha/total_abn_ha
         ) %>% 
  ungroup() %>%
  left_join(., filter(., lc_2017 == 4) %>% arrange(prop_lc_area_ha) %>% mutate(order = 1:n()) %>% select(site, order))

# save:
write_csv(abn_prop_lc_2017, file = paste0(p_dat_derived, "abn_prop_lc_2017", run_label, ".csv"))
# abn_prop_lc_2017 <- read_csv(file = paste0(p_dat_derived, run_label, "/derived_data/", "abn_prop_lc_2017", run_label, ".csv"))

  
# plot
gg_prop_lc_2017 <- ggplot(data = abn_prop_lc_2017, 
                          mapping = aes(y = fct_reorder(site, order),
                                 x = prop_lc_area_ha, 
                                 fill = as_factor(lc_2017))) +
  geom_col(position = position_dodge(), width = 0.6) + 
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Proportion of abandoned cropland area\nin each land cover class (2017)", 
       y = NULL,
       fill = "Land cover class") + 
  scale_fill_manual(values = c("#276419", "#7FBC41"), labels = c("Forest", "Grassland")) +
  theme_classic() +
  theme(legend.position = "top") + 
  scale_y_discrete(#expand = c(0.02, 0),
                   labels = c("belarus" = "Vitebsk, Belarus /\nSmolensk, Russia",
                              "bosnia_herzegovina" = "Bosnia &\n Herzegovina",
                              "chongqing" = "Chongqing, China",
                              "goias" = "Goiás, Brazil",
                              "iraq" = "Iraq",
                              "mato_grosso" = "Mato Grosso,\nBrazil",
                              "nebraska" = "Nebraska /\nWyoming, USA",
                              "orenburg" = "Orenburg, Russia /\nUralsk, Kazakhstan",
                              "shaanxi" = "Shaanxi/Shanxi,\nChina",
                              "volgograd" = "Volgograd, Russia",
                              "wisconsin" = "Wisconsin, USA"))

ggsave(plot = gg_prop_lc_2017,
       filename = paste0(p_dat_derived, "/abn_prop_lc_2017", run_label, ".pdf"), 
       width = 4.5, height = 4.5, units = "in")


### TBD ###
# ----------------------------------------------------------------------------- #
# Part II: At what age does abandoned land transition from grassland to forest?
# ----------------------------------------------------------------------------- #
# Take the abandonment age dt, and run the filter again to remove all 0s and all 1s, after the moving window filters.
# Take the raw land cover dt, and select the matching x and y, and Select a new  this as 1




# ----------------------------------------------------------------------------- #
# Part III: what land cover class is most likely to be found in newly abandoned land?
# ----------------------------------------------------------------------------- #

# what land cover classes are more likely to get recultivated? What is the LC the year prior to recultivation? 


# Update, 4.12.2021
# Two parts to this idea:
# 0. What is the distribution of abandoned land across land cover types at the end of the time series? 
# In other words, in 2017, what percentage of abandoned land is in grassland, forest, etc. 

# 1. transitions between categorical land cover classes, and 
# 1a. Which pieces of land transition from crop -> grassland -|, crop -> grassland -> forest, crop -> forest?
# 1b. How long does it take for abandoned land to transition from grassland to forest in different sites?

# 2. continuous evolution of some metrics through time post abandonment
# 2. NDVI, biomass accumulation post abandonment. 