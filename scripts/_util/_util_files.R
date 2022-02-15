# --------------------------------------------------------------- #
#
# Loading required files
# 
# --------------------------------------------------------------- #

# Switches ----
load_habitats_aoh <- FALSE
load_abn_lcc_and_masks <- FALSE

site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
site_df_w_size <- read.csv(file = paste0(p_dat_derived, "site_df_w_size.csv"))

run_label # check "_util_main.R"



# ------------------------------------------------------------ # 
# -------------------- Derived results data.frames -----------
# ------------------------------------------------------------ # 


# set run: 
run_label # check. See "_util_main.R"


# load all distilled and mean length datasets
length_distill_df <- read_csv(file = paste0(p_derived2, "length_distill_df", run_label, ".csv"))
mean_length_df <- read_csv(file = paste0(p_derived2, "mean_length_df", run_label, ".csv"))

recult_length_distill_df <- read_csv(file = paste0(p_derived2, "recult_length_distill_df", run_label, ".csv"))
mean_recult_length_df <- read_csv(file = paste0(p_derived2, "mean_recult_length_df", run_label, ".csv"))


# load summary statistic derived from lengths
summary_stats_all_sites <- read_csv(
  file = paste0(p_derived2, "summary_stats_all_sites", run_label, ".csv"))
summary_stats_all_sites_pooled <- read_csv(
  file = paste0(p_derived2, "summary_stats_all_sites_pooled", run_label, ".csv"))

recult_summary_stats_all_sites <- read_csv(
  file = paste0(p_derived2, "recult_summary_stats_all_siteS", run_label, ".csv"))
recult_summary_stats_all_sites_pooled <- read_csv(
  file = paste0(p_derived2, "recult_summary_stats_all_sites_pooled", run_label, ".csv"))


# for plotting fig 2:
length_distribution <- read_csv(file = paste0(p_derived2, "length_distribution", run_label, ".csv"))
length_distribution_max <- read_csv(file = paste0(p_derived2, "length_distribution_max", run_label, ".csv"))
recult_length_distribution <- read_csv(file = paste0(p_derived2, "recult_length_distribution", run_label, ".csv"))


# load area summary stats:
area_summary_df <- read_csv(file = paste0(p_derived2, "area_summary_df", run_label, ".csv"))

area_recult_threshold <- read_csv(file = paste0(p_derived2, "area_recult_threshold", run_label, ".csv"))


# load stats on proportion of abandoned cropland by land cover
abn_lc_area_2017 <- read_csv(file = paste0(p_derived2, "abn_lc_area_2017", run_label, ".csv"))
abn_prop_lc_2017 <- read_csv(file = paste0(p_derived2, "abn_prop_lc_2017", run_label, ".csv"))


# ------------------------------------------------------------------------------ #
# load combined persistence, area, and turnover datasets:
persistence_dat <- 
  # dat_l <- 
  read_csv(file = paste0(p_derived2, "persistence_dat", run_label, ".csv")) %>%
  mutate(site = as.factor(site),
         bins = as.factor(bins),
         year_abn_bins = as.factor(year_abn_bins),
         cohort = as.factor(cohort))

area_dat <- read_csv(file = paste0(p_derived2, "area_dat", run_label, ".csv"))
turnover_dat <- read_csv(file = paste0(p_derived2, "turnover_dat", run_label, ".csv"))
potential_area_dat <- read_csv(file = paste0(p_derived2, "potential_area_dat", run_label, ".csv"))
potential_persistence_dat <- read_csv(file = paste0(p_derived2, "potential_persistence_dat", run_label, ".csv"))


# 2 year overestimation analysis
# see chunk "prop-two-yr-abn-less-than-5-yrs" in 1_summary_stats.Rmd for this, 
# plus total_area_by_site, abn_2yr_ages_summary_df
abn_2yr_ages_df <- read_csv(file = paste0(p_derived2, "abn_2yr_ages_df", run_label, ".csv"))

abn_2yr_overestimation <- read_csv(file = paste0(p_derived2, "abn_2yr_overestimation", run_label, ".csv"))


# load data for decay models --------------------------------------------------- #

# load decay rates
# load(file = paste0(p_derived2, "/decay_mod_coef_dfs", run_label, ".rds"), verbose = TRUE)

# fitted_combo
# time_to_combo
# endpoints

# fitted_df_l3 <- read_csv(file = paste0(p_derived2, "/decay_mod_fitted_df_l3", run_label, ".csv"))
# contains: coef_df_l3, coef_df_wide_l3, mean_coef_df_l3, mean_coef_df_wide_l3, mean_coef_proj_l3, time_to_l3


# load extrapolation data
# extrapolate_df_l3 <- read_csv(file = paste0(p_derived2, "/extrapolate_df_l3", run_label, ".csv"))
# extrapolate_df_l3_a2 <- read_csv(file = paste0(p_derived2, "/extrapolate_df_l3_a2", run_label, ".csv"))
# future_df_l3 <- read_csv(file = paste0(p_derived2, "/future_df_l3", run_label, ".csv"))

# ------------------------------------------------------------ # 
# -------------------------- SF spatial files ----------------
# ------------------------------------------------------------ # 

site_sf <- st_read(paste0(p_dat_derived, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()


site_ecoregions2017 <- st_read(paste0(p_dat_derived, "sf/site_ecoregions2017.shp"))
site_biomes2017 <- st_read(paste0(p_dat_derived, "sf/site_biomes2017.shp"))

ecoregions2017_simple <- st_read(paste0(p_dat_derived, "sf/ecoregions2017_simple.shp"))
biomes2017_simple <- st_read(paste0(p_dat_derived, "sf/biomes2017_simple.shp"))


# plot(site_sf$geometry)
# plot(site_sf %>% filter(site == "shaanxi") %>% st_geometry())


# ----------------------- #
# --- pixel area (ha) --- #
# ----------------------- #
site_area_ha <- lapply(
  list.files(paste0(p_derived, "site_area_ha"), full.names = TRUE), 
  function(i) rast(i)
)
names(site_area_ha) <- site_df$site


# ------------------------------------------------------------ # 
# -------------------- Raster data --------------------
# ------------------------------------------------------------ # 

# Land use class codes:
#       1. Non-vegetated area (e.g. water, urban, barren land)
#       2. Woody vegetation
#       3. Cropland 
#       4. Herbaceous land (e.g. grassland)

# small test rasters:
# bs <- brick(paste0(p_dat, "Abandonment/belarus_small.tif"))
# bt <- brick(paste0(p_dat_derived, "belarus_subset.tif"))
# names(bs) <- paste0("y", 1987:2017)
# names(bt) <- paste0("y", 1987:2017)

# ------------------------------- load land cover maps --------------------------------------- #
# prepared input rasters (derived by Chris)

# lc <- lapply(1:11, function(i) {
#   terra::rast(paste0(p_dat_derived, "input_rasters/", site_df$site[i], ".tif"))
#   })
# names(lc) <- site_df$site

# ------------------------------- load cleaned land cover maps --------------------------------------- #
# prepared input rasters, passed through temporal filter
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "input_rasters/", site_df$site[i], "_clean.tif"))
  })
names(lcc) <- site_df$site


# ----------------------------- load abandonment age rasters ---------------------------- #

# abandonment age maps (produced by Chris)
age_t <- lapply(1:11, function(i) {
  terra::rast(
  # raster::brick(
      paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_age", run_label, ".tif")
    )
  })

names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017



# ------------------ bins ------------------ #
# age bins

age_t_bins <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/2017_bins/",
           site_df$site[i], "_y2017_bins", run_label, ".tif")
  )
})

names(age_t_bins) <- site_df$site


# ----------------------- #
# --- max_age of abandonment --- #
# ----------------------- #

max_age_t <- lapply(list.files(paste0(p_dat_derived, "max_age/", run_label), full.names = TRUE) %>% 
                      grep(".tif", ., value = TRUE), 
                    function(i) {terra::rast(i)})
names(max_age_t) <- site_df$site
for (i in seq_along(max_age_t)) {names(max_age_t[[i]]) <- "max_age"} # remember: these are just 1987:2017


# max age bins
max_age_t_bins <- lapply(list.files(paste0(p_dat_derived, "max_age/", run_label, "/bins"), full.names = TRUE), 
                         function(i) {rast(i)})
names(max_age_t_bins) <- site_df$site


# potential abandonment age maps, assuming no recultivation
potential_age_t <- lapply(1:11, function(i) {
  terra::rast(
    # raster::brick(
    paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_potential_age", run_label, ".tif")
  )
})

names(potential_age_t) <- site_df$site
for (i in seq_along(potential_age_t)) {names(potential_age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017


# ----------------------------------------------------------- #
# 3. Load the maximum extent of all cropland ever cultivated during time series
# ----------------------------------------------------------- #
lcc_total_crop_mask <- lapply(
  list.files(paste0(p_dat_derived, "total_crop_mask"), full.names = TRUE) %>%
    grep("clean", ., value = TRUE, invert = FALSE), 
  function(i) {
    rast(i)
  })

names(lcc_total_crop_mask) <- site_df$site


# ----------------------- #
# --- year of first abandonment maps (from He) ---- #
# ----------------------- #
# yoa_files <- list.files(paste0(p_dat, "Abandonment/year_of_abandonment/"))


### 

# ------------------------------------------------------------ # 
# ------------------- data.tables ---------------------  
# ------------------------------------------------------------ # 

# lc_dt
i <- 9 # set site index to load
# lc_dt <- fread(paste0(p_dat_derived, "input_data.tables/", site_df$site[i], ".csv"))
# lc_dt <- fread(paste0(p_dat_derived, "input_data.tables/", site_df$site[i], "_clean.csv"))
lc_dt <- read_parquet(paste0(p_dat_derived, "input_data.tables/", site_df$site[i], "_clean.parquet"))

# age_dt
# age_dt <- fread(input = paste0(p_dat_derived, "age_dt/", site_df$site[i], "_age", run_label,".csv"))
age_dt <- read_parquet(paste0(p_dat_derived, "age_dt/", site_df$site[i], "_age", run_label,".parquet"))

potential_age_dt <- fread(input = paste0(p_dat_derived, "age_dt/", site_df$site[i], "_potential_age", run_label,".csv"))
max_age_dt <- fread(input = paste0(p_dat_derived, "age_dt/", site_df$site[i], "_max_age", run_label,".csv"))
recult_age_dt <- fread(input = paste0(p_dat_derived, "age_dt/", site_df$site[i], "_recult_age", run_label,".csv"))


# b_age <- fread(input = paste0(p_dat_derived, "belarus_age.csv"))
# names(b_age)
# 
# s_age <- fread(input = paste0(p_dat_derived, "shaanxi_age.csv"))
# 
# 
# b_length <- fread(input = paste0(p_dat_derived, "lengths/", "belarus_length_b1.csv"))
# s_length <- fread(input = paste0(p_dat_derived, "lengths/", "shaanxi_length_b1.csv"))
# 
# b_max_length <- fread(input = paste0(p_dat_derived, "lengths/", "belarus_max_length_b1.csv"))
# s_max_length <- fread(input = paste0(p_dat_derived, "lengths/", "shaanxi_max_length_b1.csv"))
# 
# # original data
# s_dt <- fread(input = paste0(p_dat_derived, "shaanxi.csv"))
# names(s_dt) <- gsub(pattern = "andcover", replacement = "y", names(s_dt))
# 
# b_dt <- fread(input = paste0(p_dat_derived, "belarus.csv")) # caution - huge file! 8.4 GB at least. 


# -------------------------------------------------------------------------- #
# ----------- Abandonment masks and lc within of abandoned pixels ---------- 
# -------------------------------------------------------------------------- #
# load two abandonment masks for 2 yr comparison analysis:

abn_2yr_mask <- lapply(1:11, function(i) {rast(paste0(p_dat_derived, "abn_2yr/", run_label, "/",
                                                      site_df$site[i], "_abn_2yr_mask.tif"))})
names(abn_2yr_mask) <- site_df$site

abn_cc_2017_mask <- lapply(1:11, function(i) {rast(paste0(p_dat_derived, "abn_2yr/", run_label, "/",
                                                          site_df$site[i], "_abn_cc_2017_mask.tif"))})
names(abn_cc_2017_mask) <- site_df$site


if(load_abn_lcc_and_masks) {
# ----------------------- #
# --- abandonment mask (>5 years) --- #
# ----------------------- #
# see "/Users/christophercrawford/work/projects/biodiversity_abn/scripts/AOH.Rmd"

abn_mask <- lapply(1:11, function(i){
  rast(paste0(p_dat_derived, "age_rasters/", run_label, "/",
              site_df$site[i], "_abn_5_30_mask", run_label,".tif"))
})


names(abn_mask) <- site_df$site

# ----------------------- #
# --- land cover class of abandoned land --- #
# ----------------------- #
abn_lcc <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lcc/",
              site_df$site[i], "_abn_lcc.tif"))
})
names(abn_lcc) <- site_df$site

# ----------------------- #
# --- land cover class of abandoned land in 2017 only --- #
# ----------------------- #
abn_lc_2017 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lc_rasters/",
              site_df$site[i], "_abn_lc_2017.tif"))
}
)
names(abn_lc_2017) <- site_df$site
}



# ------------------------------------------------------------ # 
# ---------------- Derived Habitat Rasters --------------------------- 
# ------------------------------------------------------------ # 
if(load_habitats_aoh) {

# ----------------------- #
# -------- PNV ---------- #
# ----------------------- #
site_pnv_30 <- lapply(
  list.files(paste0(p_derived, "site_pnv"), full.names = TRUE) %>% grep("_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_pnv_30) <- site_df$site



# ----------------------- #
# -------- Jung IUCN Habitat Types ---------- #
# ----------------------- #
# level 2, at ~ 100m resolution (i.e., not resampled to 30m)
site_jung_l2 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>%
    grep("_l2_buff", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l2) <- site_df$site

# resampled to ~30 m resolution
# level 1
site_jung_l1_30 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>% grep("l1_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l1_30) <- site_df$site

# level 2
site_jung_l2_30 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>% grep("l2_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l2_30) <- site_df$site


# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv"))


# 34 habitats that occur at my sites:
site_habitats <- jung_hab_type_area_df %>%
  select(lc, habitat_type, code, Coarse_Name, IUCNLevel) %>% 
  unique() %>% arrange(habitat_type) #%>% .$code

# ----------------------- #
# ---- forest carbon ---- #
# ----------------------- #
site_forest_c_30 <- lapply(
  list.files(paste0(p_derived, "site_forest_carbon"), full.names = TRUE) %>% grep("_forest_c_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_forest_c_30) <- site_df$site


# ----------------------- #
# --- pixel area (ha) --- #
# ----------------------- #
site_area_ha <- lapply(
  list.files(paste0(p_derived, "site_area_ha"), full.names = TRUE), 
  function(i) rast(i)
)
names(site_area_ha) <- site_df$site

# ----------------------- #
# --- pixel elevation (m) --- #
# ----------------------- #
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "elevation/", 
              site_df$site[i], "_srtm_crop.tif")
  )
})
names(elevation_map) <- site_df$site





# ------------------------------------------------------------ # 
# ---------------------- IUCN Data --------------------------- 
# ------------------------------------------------------------ # 

# spatial data
# cropped range maps
load(file = paste0(p_derived, "species_ranges/vert_sites.RData"), verbose = TRUE)
load(file = paste0(p_derived, "species_ranges/species_ranges.RData"), verbose = TRUE)

# list of unique species-site combinations at my sites
species_list <- read_csv(file = paste0(p_derived, "/species_list.csv"))

iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv"))

habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))
habitat_details <- read_csv(file = paste0(p_derived, "iucn_habitat_details_subset.csv"))
species_synonyms <- read_csv(file = paste0(p_derived, "iucn_species_synonyms_subset.csv"))
common_names <- read_csv(file = paste0(p_derived, "iucn_common_names_subset.csv"))

habitat_age_req <- read_csv(file = paste0(p_derived, "iucn_habitat_age_req.csv"))

}


# ------------------------------------------------------------ # 
# ------------------- Carbon ---------------------  
# ------------------------------------------------------------ # 
site_carbon_df <- read_csv(file = paste0(p_derived2, "/carbon_df", run_label,".csv"))


# ----------------------- #
# ---- forest carbon ---- #
# ----------------------- #
# Cropped and resampled to 30 meter resolution

# aboveground forest carbon sequestration, from Cook-Patton et al. 2020 Nature
site_forest_c_abg_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_forest_c_abg_30.tif"))
})
names(site_forest_c_abg_30) <- site_df$site


# belowground forest carbon sequestration, from Cook-Patton et al. 2020 Nature
site_forest_c_blg_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_forest_c_blg_30.tif"))
})
names(site_forest_c_blg_30) <- site_df$site



# Aboveground, belowground, and average soil organic carbon sequestration in forests, combined
site_forest_c_total <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_forest_c_total.tif"))
})

names(site_forest_c_total) <- site_df$site


# ----------------------- #
# ---- Soil Revealed (SOC) ---- #
# ----------------------- #

# soils revealed, rewilding scenario, year 20
site_soils_rw_Y20_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/soils/", 
              site_df$site[i], "_soils_rw_Y20_30.tif"))
})
names(site_soils_rw_Y20_30) <- site_df$site

# soils revealed, rewilding scenario, Steady State (year 80)
site_soils_rw_YSS_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/soils/", 
              site_df$site[i], "_soils_rw_YSS_30.tif"))
})
names(site_soils_rw_YSS_30) <- site_df$site

# IPCC land cover classes, used by Soils Revealed
site_soils_lc_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/soils/", 
              site_df$site[i], "_soils_lc_30.tif"))
})
names(site_soils_lc_30) <- site_df$site


# mean soil organic carbon sequestration in former croplands
soc_mean <- read_csv(file = paste0(p_derived2, "soc_mean.csv"))

# mean soil carbon sequestration after 20 and 80 years (YSS) in former cropland, for the rewilding scenario (SoilsRevealed)
site_mean_soc <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/soils/", 
              site_df$site[i], "_mean_soc.tif"))
})
names(site_mean_soc) <- site_df$site

# --------------------------
# Savanna mask, derived from Ecoregions2017

# Forest (1), grassland (2), and savanna (3) Ecoregions2017 
site_savanna_mask <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_savanna_mask.tif"))
})
names(site_savanna_mask) <- site_df$site

# "Coarse" in that the fasterize raster derived from the Ecoregions2017 sf object was at the resolution of Cook-Patton 2020, no the 30 m resolution of the land cover maps.
site_savanna_mask_coarse <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_savanna_mask_coarse.tif"))
})
names(site_savanna_mask_coarse) <- site_df$site


# Forest carbon, masked to only forest areas (according to Ecoregions2017)
site_forest_c_masked <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/forests/", 
              site_df$site[i], "_forest_c_total_masked.tif"))
})
names(site_forest_c_masked) <- site_df$site

# Soil carbon, masked to non-forest areas.
site_soc_masked <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/soils/", 
              site_df$site[i], "_mean_soc_masked.tif"))
})
names(site_soc_masked) <- site_df$site


# Total forest and soil organic carbon, annual rates per hectare
site_forest_soc_combined <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/", 
              site_df$site[i], "_forest_soc_combined.tif"))
})
names(site_forest_soc_combined) <- site_df$site

# Total forest and soil organic carbon, annual rates per pixel (i.e., multiplied by pixel area)
site_forest_soc_combined_per_pixel <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "carbon/", 
              site_df$site[i], "_forest_soc_combined_per_pixel.tif"))
})
names(site_forest_soc_combined_per_pixel) <- site_df$site



# ------------------------------------------------------------ # 
# ----------------------------- Basemaps --------------------- 
# ------------------------------------------------------------ # 
# 
# world <- ne_countries(scale = 110, returnclass = "sf") #%>% st_make_valid() # can set returnclass to sf or sp.
# plot(world$geometry)
# 
# world_10 <- ne_countries(scale = 10, returnclass = "sf")# %>% st_make_valid() # can set returnclass to sf or sp.
# 
# # plot(world_10$geometry, graticule = TRUE, axes = TRUE)
# 
# # plot world
# # plot(world_sf$geometry)
# 
# eastern_europe <- ne_countries(scale = 110, returnclass = "sf", continent = "europe")
# # plot(eastern_europe$geometry)
# st_crs(eastern_europe$geometry)
# eastern_europe$geometry %>%
#   st_transform(., crs("+proj=longlat +ellps=WGS84 +lon_0=70")) %>%
#   plot(border = "red")
# 
# crs(usa)
# usa <- ne_countries(scale = 110, country = "United States of America", returnclass = "sf") # can set returnclass to sf or sp.
# 
# china <- ne_countries(scale = 110, returnclass = "sf", country = "china")
# # plot(china$geometry, graticule = TRUE, axes = TRUE)
# 
# # plot(usa$geometry, graticule = TRUE, axes = TRUE)
# 
# l <- eastern_europe$geometry %>% st_wrap_dateline()
# l <- st_transform(eastern_europe$geometry, "+proj=laea +lon_0=30")
# l %>%
#   plot()
# 
# plot(box$geometry)
# ecoregions
# 
# r <- b_age_r$y2017
# r
# rt <- projectRaster(r, crs = "+proj=laea")
# plot(projectRaster(extent(b_age_r), crs = "+proj=laea"), add = T, col = "red")
# plot(extent(s_age_r), add = T, col = "red")
