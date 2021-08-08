# Final land cover classes of abandoned and recultivated lands
# Christopher Crawford, March 23rd, 2021
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------------- #
# Part I: What proportion of the land that was abandoned at each site falls into the different land cover classes?
# ----------------------------------------------------------------------------- #

# 1. Easiest method is to a) take the abandonment age rasters, select 2017, then b) take the land cover rasters, select 2017, 
# 2. Merge the two
# 3. Convert to data.tables
# 4. Calculate summary statistics

# load site input land cover rasters:
# --------------- list of all sites ----------------- #
# prepared input rasters (derived by Chris)
site_input_raster_files <- list.files(paste0(p_dat_derived, "input_rasters"), full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = TRUE)

site_r <- lapply(seq_along(site_input_raster_files), function(i) {brick(site_input_raster_files[i])})
names(site_r) <- site_df$site

# rename raster layers:
for (i in 1:11) {
  if (names(site_r[i]) == "nebraska") {
    names(site_r[[i]]) <- paste0("y", 1986:2018)
  } else {
    if (names(site_r[i]) == "wisconsin") {
      names(site_r[[i]]) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      names(site_r[[i]]) <- paste0("y", 1987:2017)
    }}}

site_r

# ----------------- load abandonment age rasters ---------------- #
# abandonment age maps (produced by Chris)
age_files <- list.files(paste0(p_dat_derived, "age_rasters/", run_label), full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = FALSE)


age_r <- lapply(seq_along(age_files), function(i) {brick(age_files[i])})
names(age_r) <- site_df$site
for (i in seq_along(age_r)) {names(age_r[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017

abn_lc_count_2017 <- lapply(site_df$site, function(i) {
  # Select just the years 2017, and convert to data.tables
  r17 <- stack(
    site_r[[i]]$y2017,
    age_r[[i]]$y2017
  )
  names(r17) <- c("lc_2017", "age_2017")
  # plot(r17)
  
  # convert to data.tables
  dt17 <- as.data.table.raster(r17)

  # where age is greater than or equal to 5, what is the land cover class breakdown?
  lc_count <- dt17[age_2017 >= 5, .N, by = .(lc_2017, age_2017)] %>% 
    tibble() %>%
    arrange(lc_2017, age_2017) %>%
    mutate(site = i)
  
  lc_count
  }
) %>% bind_rows()

# save the lc_count_2017 tibble
write_csv(abn_lc_count_2017, file = paste0(p_dat_derived, run_label, "/", "abn_lc_count_2017", run_label, ".csv"))
# abn_lc_count_2017 <- read_csv(file = paste0(p_dat_derived, run_label, "/", "abn_lc_count_2017", run_label, ".csv"))


# what proportion of the abandoned land at each site ends up in forest vs. grassland?
abn_prop_lc_2017 <- abn_lc_count_2017 %>% 
  group_by(site, lc_2017) %>%
  summarise(lc_freq = sum(N)) %>%
  left_join(., 
            abn_lc_count_2017 %>% 
              group_by(site) %>%
              summarise(abn_freq = sum(N))) %>%
  mutate(prop_lc = lc_freq/abn_freq) %>% ungroup() %>%
  left_join(., filter(., lc_2017 == 4) %>% arrange(prop_lc) %>% mutate(order = 1:n()) %>% select(site, order))

# save:
write_csv(abn_prop_lc_2017, file = paste0(p_dat_derived, run_label, "/", "abn_prop_lc_2017", run_label, ".csv"))
# abn_prop_lc_2017 <- read_csv(file = paste0(p_dat_derived, run_label, "/", "abn_prop_lc_2017", run_label, ".csv"))

  
# plot
gg_prop_lc_2017 <- ggplot(data = abn_prop_lc_2017, 
                          mapping = aes(y = fct_reorder(site, order),
                                 x = prop_lc, 
                                 # group = as_factor(lc_2017), 
                                 fill = as_factor(lc_2017))) +
  geom_col(position = position_dodge(), width = 0.6) + 
  labs(x = "Proportion of abandoned cropland in \neach land cover class (2017)", y = "Site",
       fill = "Land cover class") + 
  scale_fill_manual(values = c("#276419", "#7FBC41"), labels = c("Forest", "Grassland")) +
  theme_classic() + theme(legend.position = "top")

ggsave(plot = gg_prop_lc_2017,
       filename = paste0(p_plots, run_label, "/abn_prop_lc_2017", run_label, ".pdf"), 
       width = 4.5, height = 4, units = "in")

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