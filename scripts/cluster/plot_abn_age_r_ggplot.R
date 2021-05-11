# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, May 5th, 2021

# Script to plotting Abandonment Age rasters with ggplot

# "2_analyze_abn.R" script, calculating abandonment trajectories for all sites
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster", "pryr")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived   <-    "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_input_rasters <-    "/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/network/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/network/clc6/abandonment_trajectories/raw_rasters/"



# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

# set run_label
run_label <- "_2021_03_13" # "_2021-03-05"

# load in the data.tables
age_dt_files <- list.files(paste0(p_dat_derived, "input_rasters"), full.names = TRUE) %>% 
  grep(paste0("[a-w]_age", run_label, ".csv"), ., value = T)


age_2017_dt <- lapply(1:11, function(i) {
  temp <- fread(input = age_dt_files[i], 
                select = c("x","y","y2017"),
                nrow = 1000000)
  temp
})

names(age_2017_dt) <- site_df$site

age_2017_dt <- bind_rows(age_2017_dt, .id = "site")

object_size(age_2017_dt)

# age_2017_dt <- fread(input = "/Users/christophercrawford/Downloads/abn_age_2017_test.csv")
age_2017_grid <-
  ggplot() +
  geom_tile(data = age_2017_dt, 
            mapping = aes(x = x, y = y, fill = y2017),
  ) + 
  scale_fill_viridis_c() +  
  # coord_quickmap() +
  facet_wrap(vars(site), scales = "free"
  ) + 
  labs(title = "Abandonment Duration (Years) in 2017", fill = "Age \n(years)") + 
  theme_classic() + 
  theme(axis.text = element_blank(),
        legend.position = c(0.885, 0.15), 
        legend.spacing = unit(2, units = "mm"), 
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0))


ggsave(plot = age_2017_grid,
       file = paste0(p_output, "abn_age_2017_maps_test1.pdf"),
       width = 7, height = 6, units = "in", dpi = 400)
