# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, February 4th, 2022 
# Calculating the potential age of abandoned croplands, assuming ~no~ recultivation occurred*
# *for qualifying periods of abandonment.
# -------------------------------------------------------- #


# -------------------------------------------------------- #
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


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/_util/_util_functions.R")



# array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
site <- site_df$site[indx] # set site:
site_label <- site_df$label[indx] # set label

# run_label (time stamp)
run_label <- format(Sys.time(), "_%Y_%m_%d") 


cat(fill = TRUE, "Set up site parameters:")
print(t(site_df[indx, ]))


cat(fill = TRUE, "Run label (time stamp):", run_label)


# 0. load age data.table
dt_potential_age <- fread(paste0(p_input_rasters, site, "_age", run_label, ".csv"))


# 1. calculate potential abandonment age:

cat(fill = TRUE, "i. Calculate **potential** age of abandonment for each pixel that experiences abandonment: cc_calc_potential_age()")
tic("calculated potential abandonment age")
cc_calc_potential_age(dt_potential_age)
toc(log = TRUE)


# because this "age" data.table has already been cleaned, this doesn't require any additional processing to remove all 1s, all 0s, or starting 1s


# 2. Write out cleaned abandonment age data.table
cat(fill = TRUE, "ii. Write out cleaned **potential** abandonment age data.table to:", 
    paste0(p_input_rasters, site, "_potential_age", run_label,".csv"))
tic("wrote out cleaned **potential** abandonment age data.table")
fwrite(dt_potential_age, file = paste0(p_input_rasters, site, "_potential_age", run_label,".csv"))
toc(log = TRUE)


# 3. Make diff data.table, each year subtracted by the previous year
# produces a data.table with year-to-year lagged differences (much like base::diff())
cat(fill = TRUE, "iii. Make dt_potential_diff, with year-to-year lagged differences: cc_diff_dt()")
tic("made diff")
dt_potential_diff <- cc_diff_dt(dt_potential_age)
toc(log = TRUE)


# 4. Write out dt_diff
cat(fill = TRUE, "iv. Write out dt_potential_diff to:", 
    paste0(p_input_rasters, site, "_potential_diff", run_label,".csv"))
tic("write out dt_potential_diff")
fwrite(dt_potential_diff, file = paste0(p_input_rasters, site, "_potential_diff", run_label,".csv"))
toc(log = TRUE)


# 5. Extract the length of each period of abandonment
cat(fill = TRUE, "v. Extract the length of each period of abandonment: cc_extract_length()")
tic("extracted length")
potential_length <- cc_extract_length(dt_potential_diff, length_type = "abandonment")
toc(log = TRUE)


# 6. Create a data.table listing the lengths, write to file.
cat(fill = TRUE, "vi. Create a data.table listing the lengths, write to:", 
    paste0(p_input_rasters, site, "_potential_length", run_label,".csv"))
tic("created data.table of lengths")
potential_length <- data.table(length = potential_length)
toc(log = TRUE)


tic("wrote out length data.table")
fwrite(potential_length, file = paste0(p_input_rasters, site, "_potential_length", run_label,".csv"))
toc(log = TRUE)



# 7.
# ---------------- save potential age data.table as a SpatRaster: ----------------
cc_save_dt_as_raster(site = site,
                     type = paste0("_potential_age", run_label),
                     input_path = p_input_rasters,
                     output_path = p_input_rasters)


# 8.
# ---------------- summarize the new potential abandonment data.tables -----------
# adapted from cc_calc_area_per_lc_abn() & cc_summaize_abn_dts

# load files:
lc_r <- terra::rast(x = paste0(p_input_rasters, site, "_clean.tif"))[[1]]
lc_dt <- fread(input = paste0(p_input_rasters, site, "_clean.csv")) 
potential_age_dt <- fread(input = paste0(p_input_rasters, site, "_potential_age", run_label,".csv"))
abandonment_threshold <- 5

cat("calculating total area in each land cover class, and that is abandoned (for at least as long as the abandonment threshold), over time.", fill = TRUE)
# ------------- calculate total area of potential abandonment ---------------- #
area_raster <- lc_r %>%
  terra::cellSize(., unit = "km", mask = FALSE) # calculate cell areas

area_dt <- spatraster_to_dt(area_raster)
setnames(area_dt, old = "area", new = "pixel_area") # update layer names

# round x and y columns to deal with floating point imprecision:
round_digits1 <- 11

while(merge(x = potential_age_dt[, 1:3], y = area_dt, 
            all.x = TRUE, by = c("x", "y"), sort = FALSE
            )[is.na(pixel_area), .N] > 0) {
  round_digits1 <- round_digits1 - 1
  potential_age_dt[, ':='(x = round(x, round_digits1), y = round(y, round_digits1))]
  area_dt[, ':='(x = round(x, round_digits1), y = round(y, round_digits1))]
}
cat(fill = TRUE, "Note: potential_age_dt x and y columns rounded to", round_digits1, "digits to facilitate merge.")

# merge area to potential_age_dt and lc_dt, based on the x and y coordinates, 
# saving only those rows in area_dt that match potential_age_dt and lc_dt, respectively:
potential_age_dt <- merge(x = potential_age_dt, y = area_dt, all.x = TRUE, 
                    by = c("x","y"), sort = FALSE)

if(potential_age_dt[is.na(pixel_area), .N] > 0 ) {stop("Merge did not match at 100%")}


abandoned_area_df_threshold <- tibble(
  year = 1987:2017,
  lc = paste0("Abandoned (>=", abandonment_threshold, ")"),
  area_ha = sapply(1:31, function(i) {
    potential_age_dt[get(paste0("y", 1987:2017)[i]) >= abandonment_threshold, 
               sum(pixel_area) * 100]
  }
  )
)

abandoned_area_df_all <- tibble(
  year = 1987:2017,
  lc = "Abandoned (>=1)",
  area_ha = sapply(1:31, function(i) {
    potential_age_dt[get(paste0("y", 1987:2017)[i]) > 0, 
               sum(pixel_area) * 100]
  }
  )
)

# return the tibble
potential_area_df <- bind_rows(abandoned_area_df_threshold, abandoned_area_df_all)


# save individual results_df
write_csv(potential_area_df, file = paste0(p_input_rasters, site, "_result_potential_area", run_label,".csv"))
cat(fill = TRUE, "Done! Wrote potential_area_df to", paste0(p_input_rasters, site, "_result_potential_area", run_label,".csv"))



# calculate persistence, to allow us to calculate age bins:
potential_persistence <- cc_calc_persistence(
  abn_age_dt = potential_age_dt,
  land_cover_raster = lc_r,
  abandonment_threshold = abandonment_threshold
  )

# save to file
write_csv(potential_persistence, file = paste0(p_input_rasters, site, "_result_potential_persistence", run_label,".csv"))


tic.log()

# end!
