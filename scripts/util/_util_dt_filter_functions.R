# --------------------------------------------------------------- #
#
# abandonment data.table filtering functions
# 
# --------------------------------------------------------------- #


# data.table filtering functions ----
# -------------------------------------------------------------------------- #
# Update land cover classes values
# -------------------------------------------------------------------------- #
cc_update_lc <- function(dt, crop_code = 0, noncrop_code = 1) {
  
  # Original Land-use class codes:
  #       1. Non-vegetated area (e.g. water, urban, barren land)
  #       2. Woody vegetation
  #       3. Cropland 
  #       4. Herbaceous land (e.g. grassland)
  
  # this function updates these land cover classes to:
  #       1. for crop
  #       2. for noncrop

  # check if data.table contains x, y columns
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
      } 
    
    for (x in names(dt[, 3:length(dt)])) {    # can also use names(dt[, !c("x", "y")])
      set(dt, i = which(dt[[x]] == 0), j = x, value = NA)       
      # set 0 values to NA
      } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[x]] == 1), j = x, value = NA)       
      # set nonveg (urban, water, etc.) to NA
      } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[x]] == 2), j = x, value = noncrop_code)      
      # set 2 (woody) to noncrop_code
      # combining into a single noncrop layer
      }
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[x]] == 3), j = x, value = crop_code)      
      # set crop from 3 to crop_code
      } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[x]] == 4), j = x, value = noncrop_code)      
      # set 4 (grassland) to noncrop_code
      # combining into a single noncrop layer
      }
    
    } else { 
      # If the data.table doesn't have x y coordinates, use the following:
      # might be slightly faster if x and y are removed first, allowing the following
      
      for (x in seq_len(length(dt))) {
        set(dt, i = which(dt[[x]] == 0), j = x, value = NA)   
        # set 0 values to NA
        }     
      
      for (x in seq_len(length(dt))) {
        set(dt, i = which(dt[[x]] == 1), j = x, value = NA)   
        # set nonveg (urban, water, etc.) to NA
        }
      
      for (x in seq_len(length(dt))) {
        set(dt, i = which(dt[[x]] == 2), j = x, value = noncrop_code)    
        # set 2 (woody) to noncrop_code
        # combining into a single noncrop layer
        }
      
      for (x in seq_len(length(dt))) {
        set(dt, i = which(dt[[x]] == 3), j = x, value = crop_code)    
        # set crop from 3 to crop_code
        }
      
      for (x in seq_len(length(dt))) {
        set(dt, i = which(dt[[x]] == 4), j = x, value = noncrop_code)    
        # set 4 (grassland) to noncrop_code
        # combining into a single noncrop layer
        }
    }
}




# -------------------------------------------------------------------------- #
# Make data.table binary 
# -------------------------------------------------------------------------- #
cc_make_dt_binary <- function(dt) {
  
  # this function takes a data.table with 1s and 2s and simply subtracts one, 
  # creating a binary (non-crop or not) data.table
  #       1. crop -> 0
  #       2. noncrop -> 1
  
  if (length(grep("^[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } 
    for (i in 3:length(dt)) {
      dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1]
      }
    } else {
      for (i in seq_len(length(dt))) {
        dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1] 
      }
    }
}

# alternatively: dt <- dt - 1 works just fine




# -------------------------------------------------------------------------- #
# Filter out pixels that are either all crop or all noncrop
# -------------------------------------------------------------------------- #
cc_remove_non_abn <- function(dt) {
  dt[dt[, rowSums(.SD) > 0 & rowSums(.SD) < length(.SD), 
        .SDcols = grep("[xy]$", names(dt), invert = TRUE)], ] 
  
  # must be used in the format:
  # dt <- cc_remove_non_abn(dt) 
  # this is inefficient with memory, but since I'll be in adroit anyways, it'll probably be fine.
  
  # all crop row sums = 0
  # all noncrop row sums = length(dt) (i.e. the number of columns)
}



# -------------------------------------------------------------------------- #
# Calculate age of each noncrop cell
# -------------------------------------------------------------------------- #
cc_calc_age <- function(dt) {
  
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    }
    start <- 4
  } else {
    start <- 2
  }
  
  for (i in start:ncol(dt)) {
    
    # subset rows that are greater than 0 (i.e. 1, for noncrop), and
    dt[get(names(dt)[i]) > 0, 
       
       # set them equal to the previous column's value in that row, plus 1.
       names(dt)[i] := get(names(dt)[i-1]) + 1] 
  }
}



# -------------------------------------------------------------------------- #
# Erase noncrop non-abn periods
# -------------------------------------------------------------------------- #
# set any value that is equal to the column number to 0. 
# This removes age values for noncrop vegetation that start time-series as noncrop - 
# this can't be classified as abandonment, since we don't know what came before the time-series.

cc_erase_non_abn_periods <- function(dt) {
  
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    }
    adjustment <- 2
  } else {
    adjustment <- 0
  }
  
  # iterate across columns
  for (i in seq_len(length(dt) - adjustment)) {
    dt[get(names(dt)[i + adjustment]) == i,  # filter rows with values equal to column number
       names(dt)[i + adjustment] := 0] # set value to 0
  }
  
}


# -------------------------------------------------------------------------- #
#  Make diff
# -------------------------------------------------------------------------- #
cc_diff_dt <- function(dt){
  # produces a data.table with year-to-year lagged differences (much like base::diff())
  
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    }
    dt_lead <- copy(dt[, -c("x", "y")])
    dt_lead[, names(dt_lead)[1] := NULL][, end := 0]
    dt_lead - dt[, -c("x", "y")]
  } else {
    dt_lead <- copy(dt)
    dt_lead[, names(dt_lead)[1] := NULL][, end := 0]
    dt_lead - dt
  }
}


# -------------------------------------------------------------------------- #
# Extract lengths of all abandonment periods
# -------------------------------------------------------------------------- #

cc_extract_length <- function(dt_diff) {
  # note: this function only works with a diff'd data.table, so that 
  # negative values mark years of recultivation (or the end of the time-series)
  abn_length <- vector(mode = "numeric")
  for(i in seq_len(length(dt_diff))) {
    abn_length <- c(abn_length, 
                     dt_diff[get(names(dt_diff)[i]) < 0,
                             get(names(dt_diff)[i])])
  }
  abs(abn_length)
}


# remove NAs from data.table
# f_dowle2 <- function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), (i):=0]
# }


# -------------------------------------------------------------------------- #
# Extract lengths of all abandonment periods
# -------------------------------------------------------------------------- #

cc_process_rasters <- function(input_raster_file, name, path_out = p_dat_derived,
                               gsub_pattern = "andcover") {
  # requires raster, data.table, devtools, tictoc, dtraster
  tic.clearlog()
  
  tic("full script processing time")
  # load raster
  tic("load raster")
  r <- brick(input_raster_file)
  if (nlayers(r) != 31) {
    stop("Raster does not have 31 layers.")
  }
  toc(log = TRUE)
  
  # update raster layer names
  tic("update raster layer names")
  # names(r) <- gsub(gsub_pattern, "y", names(r))
  names(r) <- paste0("y", 1987:2017) # 
  toc(log = TRUE)
  
  # load as a data.table - warning, this is very slow
  tic("load as a data.table")
  print("converting raster to data.table...")
  dt <- as.data.table.raster(r)
  print("done: data.table converted")
  toc(log = TRUE)
  
  # write out data.table as a csv
  tic("write out data.table as a csv")
  print("writing data.table to csv")
  fwrite(dt, file = paste0(path_out, name, ".csv"))
  toc(log = TRUE)
  
  # start heavy processing
  tic("update land cover classes")
  cc_update_lc(dt, crop_code = 0, noncrop_code = 1)  # update land cover classes
  toc(log = TRUE)
  
  tic("remove NAs")
  dt <- na.omit(dt)   # remove NAs
  toc(log = TRUE)
  
  tic("filter non abandonment pixels")
  dt <- cc_remove_non_abn(dt)   # filter non abandonment pixels
  toc(log = TRUE)
  
  tic("calculate age")
  cc_calc_age(dt)  # calculate age
  toc(log = TRUE)
  
  tic("erase non abandonment periods")
  cc_erase_non_abn_periods(dt)  # erase non abandonment periods
  toc(log = TRUE)
  
  # write out cleaned abandonment age data.table
  tic("write out cleaned abandonment age data.table")
  print("writing out cleaned abandonment age data.table")
  fwrite(dt, file = paste0(path_out, name, "_age.csv"))
  toc(log = TRUE)
  
  # make diff
  tic("make diff")
  dt_diff <- cc_diff_dt(dt)
  toc(log = TRUE)
  
  # write out dt_diff
  tic("write out dt_diff")
  print("writing out dt_diff")
  fwrite(dt_diff, file = paste0(path_out, name, "_diff.csv"))
  toc(log = TRUE)
  
  # extract length
  tic("extract length")
  length <- cc_extract_length(dt_diff)
  toc(log = TRUE)
  
  # write out length
  tic("write out length")
  length <- data.table(length = length)
  toc(log = TRUE)
  
  tic("write out length data.table")
  print("writing out length data.table")
  fwrite(length, file = paste0(path_out, name, "_length.csv"))
  toc(log = TRUE)
  
  
  toc(log = TRUE) # final toc
  
  print(paste0("Done: ", name))
  print(tic.log())
}



# -------------------------------------------------------------------------- #
# save processed data.tables showing age of abandonment as rasters
# -------------------------------------------------------------------------- #
cc_save_age_rasters <- function(name, directory = p_dat_derived) {
  # load dt
  dt <- fread(file = paste0(directory, name, "_age.csv"))
  
  # convert age dt to raster
  r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))

  # write raster
  writeRaster(r, filename = paste0(directory, name, "_age.tif"))
  
  # reload, and assign
  # brick(paste0(directory, name, "_age.tif"))
}




# -------------------------------------------------------------------------- #
# calculate the maximum length of time abandoned (max age) for each pixel 
# -------------------------------------------------------------------------- #
cc_calc_max_age <- function(dt, directory = p_dat_derived, name) {
  
  dt[, max_length := max(.SD), .SDcols = -c("x", "y"), by = .(x, y)]
  
  # write out just the max dt.
  fwrite(dt[, .(x, y, max_length)], file = paste0(directory, name, "_max_length.csv"))
  
}


# summary functions ---- 
# -------------------------------------------------------------------------- #
# calculate the total area in:
# each land cover class in the original land cover data, and
# that is abandoned, over time
# -------------------------------------------------------------------------- #
cc_calc_area_per_lc_abn <- function(land_cover_dt, abn_age_dt, land_cover_raster) {
  col_names <- grep("x$|y$", names(land_cover_dt), value = TRUE, invert = TRUE)
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  median_cell_area_km2 <- median(getValues(area_raster))
  
  for (i in seq_along(col_names)) {
    temp_dt <- land_cover_dt[, .N, by = c(col_names[i])][order(get(col_names[i]))]
    setnames(temp_dt, old = col_names[i], new = "lc")
    setnames(temp_dt, old = "N", new = col_names[i])
    
    if(i>1) {
      lc_area_df <- merge(lc_area_df, temp_dt, by = "lc")
    } else {
      lc_area_df <- temp_dt
    }
  }
  
  # now, recode and pivot
  lc_area_df <- lc_area_df %>% 
    as_tibble() %>%
    filter(lc != 0) %>% 
    mutate(lc = as_factor(lc),
           lc = recode(lc, # old = new
                       "1" = "Non-veg", 
                       "2" = "Woody veg",
                       "3" = "Crop", 
                       "4" = "Grassland")
    ) %>%
    pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "count") %>%
    mutate(year = as.integer(gsub("y", "", year))) %>%
    select(year, lc, count)
  
  # calculate area of each category of land cover, 
  # based on the median cell size
  lc_area_df <- lc_area_df %>%
    mutate(area_ha = count * median_cell_area_km2 * 100)
  
  
  abandoned_area_df <- tibble(
    year = 1987:2017,
    lc = "Abandoned",
    count = sapply(1:31, function(i) {age_dt[get(paste0("y", 1987:2017)[i]) > 0, .N]}),
    area_ha = count * median_cell_area_km2 * 100
  )
  
  # return the tibble
  area_df <- rbind(lc_area_df, abandoned_area_df)
  
}

# ------------------------------------------------------------------------------------ #
# calculate area of abandoned land over time, for a particular age_dt
# ------------------------------------------------------------------------------------ #

cc_calc_abn_area <- function(age_dt, area_raster) {
  abandoned_area_df <- tibble(
    year = 1987:2017,
    lc = "Abandoned",
    count = sapply(1:31, function(i) {age_dt[get(paste0("y", 1987:2017)[i]) > 0, .N]}),
    area_ha = count * median(getValues(area_raster)) * 100
  )
}

# ------------------------------------------------------------------------------------ #
# calculate persistence of abandoned land over time, either as a raw count or as a percentage
# ------------------------------------------------------------------------------------ #
cc_calc_persistence <- function(age_dt, land_cover_raster,
                                stat_proportion = TRUE, pivot_to_long = TRUE, NA_first = FALSE) {
  
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  median_cell_area_km2 <- median(getValues(area_raster))
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  persistence_list <- lapply(1:30, function(j) {
    # calculate a vector of counts of abandoned pixels that originate in a particular year, 
    # and the count of those pixels in each year following, starting in 1988.
    # Filled with NAs for periods that are beyond the length of the time series.
    
    temp_vector <- c(
      if (NA_first) {rep(NA, j)} else {rep(NA, 0)},
      sapply(1:(31 - j), function(i) {
        age_dt[get(paste0("y", 1987:2017)[i + j]) == i, .N]
      }),
      if (NA_first) {rep(NA, 0)} else {rep(NA, j)}
    )
    
    # convert this vector to a proportion of original cohort of abandoned pixels
    if(stat_proportion) {
      temp_vector / max(temp_vector, na.rm = TRUE) # calculate as percentage
    } else {
      temp_vector
    }
  }
  )
  
  # combine list into a data.frame
  persistence_df <- data.frame(
    do.call("cbind", persistence_list))
  
  names(persistence_df) <- paste0("y", 1988:2017)
  
  persistence_df <- persistence_df %>%
    mutate(time_abn = 
             if(NA_first) {1987:2017
               } else {
                 1:31
                 }
           ) %>%
    select(time_abn, everything())
  
  if(NA_first) {
    names(persistence_df)[1] <- "year"
  } else {
      persistence_df
    }
  
  # pivot to long format
  if(pivot_to_long) {
    persistence_long <- persistence_df %>%
      pivot_longer(cols = paste0("y", 1988:2017), 
                   names_to = "year_abn", 
                   values_to = if (stat_proportion) {"proportion"} else {"count"},
                   values_drop_na = TRUE) %>%
      mutate(year_abn = as.integer(gsub("y", "", year_abn))) 
    
    if (stat_proportion) {
      persistence_long
    } else {mutate(persistence_long, area_ha = count * median_cell_area_km2 * 100)}
    
  } else {
    persistence_df
  }
  # if(pivot_to_long) {persistence_long} else {persistence_df}
  
}


# ------------------------------------------------------------------------------------ #
# calculate gains and losses of abandoned land over time
# ------------------------------------------------------------------------------------ #
cc_calc_abn_area_diff <- function(age_dt, area_raster) {
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  abn_turnover_list <- lapply(1:30, function(j) {
    # Calculate a vector of counts of abandoned pixels that originate in a particular year, 
    # and the count of those pixels in each year following, starting in 1988.
    # Filled with 0s for periods that are beyond the length of the time series.
    # Then, calculate the difference year-to-year for each abandonment cohort.
    
    temp_vector_diff <- c(
      rep(0, j),
      sapply(1:(31 - j), function(i) {
        age_dt[get(paste0("y", 1987:2017)[i + j]) == i, .N]
      })
    ) %>%
      diff()
    }
    )
  
  # combine list into a data.frame
  abn_turnover_df <- data.frame(
    do.call("cbind", abn_turnover_list))
  
  names(abn_turnover_df) <- paste0("y", 1988:2017)
  
  abn_turnover_df <- abn_turnover_df %>%
    mutate(year = 1988:2017) %>%
    select(year, everything())
  
  # pivot to long format
  abn_turnover_long <- abn_turnover_df %>%
    pivot_longer(cols = paste0("y", 1988:2017), 
                 names_to = "year_abn", 
                 values_to = "count",
                 values_drop_na = TRUE) %>%
    mutate(year_abn = as.integer(gsub("y", "", year_abn)))
  
  
  # calculate net, gain, and loss:
  # net gain in abandoned land area
  abn_area_net <- abn_turnover_long %>%
    group_by(year) %>% 
    summarise(net = sum(count))
  
  # gain
  abn_area_gain <- abn_turnover_long %>%
    group_by(year) %>% 
    filter(count > 0) %>%
    summarise(gain = sum(count))
  
  # loss
  abn_area_loss <- abn_turnover_long %>%
    group_by(year) %>% 
    filter(count < 0) %>%
    summarise(loss = sum(count))
  
  abn_area_change_df <- abn_area_net %>% 
    full_join(., abn_area_gain, by = "year") %>%
    full_join(., abn_area_loss, by = "year") %>%
    pivot_longer(cols = c("net", "gain", "loss"),
                 names_to = "direction", values_to = "count",
                 values_drop_na = TRUE) %>%
    mutate(area_ha = count * median(getValues(area_raster)) * 100)
  
  abn_area_change_df

}




# plot and save functions ----

# ------------------------------------------------------------------------------------ #
# plot histograms
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot lc and abandonment area over time
# ------------------------------------------------------------------------------------ #
cc_save_plot_lc_abn_area <- function(df, subtitle, outfile_label) {
  
  gg_lc_abn_area <- ggplot(data = df, aes(year, area_ha)) +
    theme_classic() +
    # theme(axis.text.x = element_text(angle = 320, vjust = 1, hjust = 0)) +
    labs(y = expression("Area  (10"^{6}*" ha)") , 
         x = "Year", 
         title = "Area by Land Cover, with Abandonment",
         subtitle = subtitle,
         color = "Land Cover") + 
    geom_line(mapping = aes(x = year, y = area_ha / (10^6), # convert to millions of ha
                            group = lc, color = lc),
              size = 1.2) + 
    scale_x_continuous(n.breaks = 10)
  
  # save
  png(filename = paste0("/Users/christophercrawford/Google Drive/_Projects/abandonment_trajectories/output/plots/lc_abn_area_", 
                        outfile_label, ".png"), 
      width = 7, height = 5, units = "in", res = 400)
  
  print(gg_lc_abn_area)
  dev.off()
}

# ------------------------------------------------------------------------------------ #
# plot persistence of abandonment over time
# ------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------ #
# plot gains and losses of abandoned land, over time
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot area of abandonment, by age class
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot map: land cover
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot map: age of abandonment
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot map: max age of abandonment cover
# ------------------------------------------------------------------------------------ #





