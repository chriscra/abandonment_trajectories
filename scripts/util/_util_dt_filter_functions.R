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


# area/persistence functions ---- 
# -------------------------------------------------------------------------- #
# calculate the total area in:
# each land cover class in the original land cover data, and
# that is abandoned, over time
# -------------------------------------------------------------------------- #
cc_calc_area_per_lc_abn <- function(land_cover_dt, abn_age_dt, land_cover_raster, 
                                    abandonment_threshold = 5) {
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
                       "1" = "Non-veg.", 
                       "2" = "Woody veg.",
                       "3" = "Cropland", 
                       "4" = "Grassland")
    ) %>%
    pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "count") %>%
    mutate(year = as.integer(gsub("y", "", year))) %>%
    select(year, lc, count)
  
  # calculate area of each category of land cover, 
  # based on the median cell size
  lc_area_df <- lc_area_df %>%
    mutate(area_ha = count * median_cell_area_km2 * 100)
  
  
  abandoned_area_df_threshold <- tibble(
    year = 1987:2017,
    lc = paste0("Abandoned (>", abandonment_threshold, ")"),
    count = sapply(1:31, function(i) {abn_age_dt[get(paste0("y", 1987:2017)[i]) >= abandonment_threshold, .N]}),
    area_ha = count * median_cell_area_km2 * 100,
  )
  
  abandoned_area_df_all <- tibble(
    year = 1987:2017,
    lc = "Abandoned (>1)",
    count = sapply(1:31, function(i) {abn_age_dt[get(paste0("y", 1987:2017)[i]) > 0, .N]}),
    area_ha = count * median_cell_area_km2 * 100,
  )
  
  
  # return the tibble
  area_df <- bind_rows(lc_area_df, abandoned_area_df_threshold, abandoned_area_df_all)
  
}

# ------------------------------------------------------------------------------------ #
# calculate area of abandoned land over time, for a particular abn_age_dt
# ------------------------------------------------------------------------------------ #

cc_calc_abn_area <- function(abn_age_dt, land_cover_raster, abandonment_definition = 5) {
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  median_cell_area_km2 <- median(getValues(area_raster))

  abandoned_area_df <- tibble(
    year = 1987:2017,
    lc = "Abandoned",
    count = sapply(1:31, function(i) {abn_age_dt[get(paste0("y", 1987:2017)[i]) >= abandonment_threshold, .N]}),
    count_all = sapply(1:31, function(i) {abn_age_dt[get(paste0("y", 1987:2017)[i]) > 0, .N]}),
    
    area_ha = count * median_cell_area_km2 * 100,
    area_all_ha = count_all * median_cell_area_km2 * 100
  )
  
}



# ------------------------------------------------------------------------------------ #
# calculate persistence of abandoned land over time, either as a raw count or as a percentage
# ------------------------------------------------------------------------------------ #
cc_calc_persistence <- function(abn_age_dt, 
                                land_cover_raster,
                                stat_proportion = TRUE, 
                                NA_first = FALSE,
                                include_wide = FALSE,
                                abandonment_threshold = 5) {
  
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  median_cell_area_km2 <- median(getValues(area_raster))
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  persistence_list <- lapply(1:30, function(j) {
    # calculate a vector of counts of abandoned pixels that originate in a particular year, 
    # and the count of those pixels in each year following, starting in 1988.
    # Filled with NAs for periods that are beyond the length of the time series.
    
    # old
    temp_vector <- c(
      if (NA_first) {rep(NA, j)} else {rep(NA, 0)},
      sapply(1:(31 - j), function(i) {
        abn_age_dt[get(paste0("y", 1987:2017)[i + j]) == i, .N]
      }),
      if (NA_first) {rep(NA, 0)} else {rep(NA, j)}
    )
    
    # new. remove cells that are below threshold
    
    if (abandonment_threshold > 1) {
      if (NA_first){
        temp_vector[c((1:(abandonment_threshold - 1)) + j)] <- NA
      } else {
        temp_vector[c(1:(abandonment_threshold - 1))] <- NA
      }
      
      if(length(temp_vector) > 31){
        temp_vector <- temp_vector[c(1:31)]
      }
    }
    
    # convert this vector to a proportion of original cohort of abandoned pixels
    if(stat_proportion) {
      if(sum(temp_vector, na.rm = TRUE) > 0) {
        temp_vector / max(temp_vector, na.rm = TRUE) # calculate as proportion
      } else {
          temp_vector
        }
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
    mutate(time_abn = if(NA_first) {1987:2017} else {1:31}) %>%
    select(time_abn, everything())
  
  if(NA_first) {names(persistence_df)[1] <- "year"} else {persistence_df}
  
  # pivot to long format
  persistence_long <- persistence_df %>%
    pivot_longer(cols = paste0("y", 1988:2017), 
                 names_to = "year_abn", 
                 values_to = if (stat_proportion) {"proportion"} else {"count"},
                 values_drop_na = TRUE) %>%
    mutate(year_abn = as.integer(gsub("y", "", year_abn))) 
  
  if (stat_proportion) {
    persistence_long <- persistence_long
  } else {
    persistence_long <- persistence_long %>%
      mutate(area_ha = count * median_cell_area_km2 * 100)
  }
  
  # add age bins, if NA_first:
  if (NA_first) {
    persistence_long <- persistence_long %>% 
      mutate(age = year - year_abn + 1) %>% 
      mutate(bins = ifelse(age > 0 & age < 5, "1 to 5 years",
                           ifelse(age >= 5 & age < 10, "5 to 10 years",
                                  ifelse(age >= 10 & age < 15, "10 to 15 years",
                                         ifelse(age >= 15 & age < 20, "15 to 20 years",
                                                ifelse(age >= 20 & age < 25, "20 to 25 years", 
                                                       ifelse(age >= 25 & age <= 30, "25 to 30 years", NA)
                                         )))))) %>%
      mutate(bins = as_factor(bins))
  } else {
    persistence_long <- persistence_long
  }
  
  
  # return product
  if (include_wide) {
    list(
      long = persistence_long,
      wide = persistence_df
    )
  } else {
    persistence_long
  }
  
}


# ------------------------------------------------------------------------------------ #
# save all permutations of abandonment persistence
# ------------------------------------------------------------------------------------ #

cc_calc_persistence_all <- function(abn_age_dt, 
                                    land_cover_raster, 
                                    include_wide = FALSE,
                                    abandonment_threshold = 5,
                                    include_all_abandonment = TRUE){

  count <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                               land_cover_raster = land_cover_raster, 
                               stat_proportion = FALSE, NA_first = FALSE, 
                               include_wide = include_wide,
                               abandonment_threshold = abandonment_threshold)
  
  proportion <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                    land_cover_raster = land_cover_raster, 
                                    stat_proportion = TRUE, NA_first = FALSE, 
                                    include_wide = include_wide,
                                    abandonment_threshold = abandonment_threshold)
  
  count_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                        land_cover_raster = land_cover_raster, 
                                        stat_proportion = FALSE, NA_first = TRUE, 
                                        include_wide = include_wide,
                                        abandonment_threshold = abandonment_threshold)
  
  proportion_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                             land_cover_raster = land_cover_raster, 
                                             stat_proportion = TRUE, NA_first = TRUE, 
                                             include_wide = include_wide,
                                             abandonment_threshold = abandonment_threshold)
  
  if(include_all_abandonment) {
    count_all <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                 land_cover_raster = land_cover_raster, 
                                 stat_proportion = FALSE, NA_first = FALSE, 
                                 include_wide = include_wide,
                                 abandonment_threshold = 1)
    
    proportion_all <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                      land_cover_raster = land_cover_raster, 
                                      stat_proportion = TRUE, NA_first = FALSE, 
                                      include_wide = include_wide,
                                      abandonment_threshold = 1)
    
    count_all_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                          land_cover_raster = land_cover_raster, 
                                          stat_proportion = FALSE, NA_first = TRUE, 
                                          include_wide = include_wide,
                                          abandonment_threshold = 1)
    
    proportion_all_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
                                               land_cover_raster = land_cover_raster, 
                                               stat_proportion = TRUE, NA_first = TRUE, 
                                               include_wide = include_wide,
                                               abandonment_threshold = 1)
    
  }

  # return the products as a list
  if(include_wide) {
    c(
      list(
        count = count,
        count_na_first = count_na_first,
        proportion = proportion,
        proportion_na_first = proportion_na_first
         ),
      if(include_all_abandonment) {
        list(
          count_all = count_all,
          count_all_na_first = count_all_na_first,
          proportion_all = proportion_all,
          proportion_all_na_first = proportion_all_na_first
        )
      }
      )
    } else {
      # join the dfs
      
      na_last <- left_join(x = count, y = proportion, 
                         by = c("time_abn", "year_abn"))
      na_first <- left_join(x = count_na_first, y = proportion_na_first, 
                          by = c("year", "year_abn", "age", "bins"))
      
      if(include_all_abandonment) {
        na_last_all <- left_join(x = count_all, y = proportion_all, 
                             by = c("time_abn", "year_abn"))
        na_first_all <- left_join(x = count_all_na_first, y = proportion_all_na_first, 
                              by = c("year", "year_abn", "age", "bins"))
      }
    
      # return list
      c(
        list(na_last = na_last,
             na_first = na_first),
        if(include_all_abandonment) {
          list(
            na_last_all = na_last_all,
            na_first_all = na_first_all
          )
          }
      )
    }
}


# ------------------------------------------------------------------------------------ #
# calculate gains and losses of abandoned land over time
# ------------------------------------------------------------------------------------ #
cc_calc_abn_area_diff <- function(abn_age_dt, land_cover_raster,
                                  abandonment_threshold = 5) {
  
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  median_cell_area_km2 <- median(getValues(area_raster))
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  abn_turnover_list <- lapply(1:30, function(j) {
    # Calculate a vector of counts of abandoned pixels that originate in a particular year, 
    # and the count of those pixels in each year following, starting in 1988.
    # Filled with 0s for periods that are beyond the length of the time series.
    # Then, calculate the difference year-to-year for each abandonment cohort.
    
    temp_vector_diff <- c(
      rep(0, j),
      sapply(1:(31 - j), function(i) {
        abn_age_dt[get(paste0("y", 1987:2017)[i + j]) == i, .N]
      })
    ) 
    
    # need to remove cells below abandonment threshold
    if (abandonment_threshold > 1) {
      temp_vector_diff[c(j + 1:(abandonment_threshold - 1))] <- 0
    }
    
    # trim to right length
    if(length(temp_vector_diff) > 31){
      temp_vector_diff <- temp_vector_diff[c(1:31)]
    }
    
    # calculate diff
    temp_vector_diff <- diff(temp_vector_diff)
    
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
    mutate(area_ha = count * median_cell_area_km2 * 100)
  
  abn_area_change_df
  
}


# generate the files needed to calculate plots
cc_generate_dfs <- function(land_cover_dt,
                            abn_age_dt, 
                            land_cover_raster, 
                            outfile_label,
                            abandonment_threshold = 5,
                            include_all = FALSE) {
  # ------------- calculate total area per lc, with abandonment ---------------- #
  area <- cc_calc_area_per_lc_abn(land_cover_dt = land_cover_dt, 
                                  abn_age_dt = abn_age_dt, 
                                  land_cover_raster = land_cover_raster,
                                  abandonment_threshold = abandonment_threshold)
  
  # ------------------------ abandonment persistence --------------------------- #
  persistence_list <- cc_calc_persistence_all(abn_age_dt = abn_age_dt, 
                                              land_cover_raster = land_cover_raster,
                                              include_wide = FALSE,
                                              abandonment_threshold = abandonment_threshold,
                                              include_all_abandonment = include_all)
  
  # -------------------- calculate the abandonment area turnover ------------------- #
  abn_area_change <- cc_calc_abn_area_diff(abn_age_dt = abn_age_dt, 
                                           land_cover_raster = land_cover_raster,
                                           abandonment_threshold = abandonment_threshold)
  
  if(include_all) {
    abn_area_change_all <- cc_calc_abn_area_diff(
      abn_age_dt = abn_age_dt, 
      land_cover_raster = land_cover_raster,
      abandonment_threshold = 1) %>% 
      
      # rename columns
      rename(count_all = count, area_ha_all = area_ha)
    
    # join
    abn_area_change <- full_join(x = abn_area_change, y = abn_area_change_all, 
                                 by = c("year", "direction"))
  }
  
  # change names
  assign(paste0("area", outfile_label), area)
  assign(paste0("persistence_list", outfile_label), persistence_list)
  assign(paste0("abn_area_change", outfile_label), abn_area_change)
  
  # save files
  save(list = c(paste0("area", outfile_label), 
                paste0("persistence_list", outfile_label),
                paste0("abn_area_change", outfile_label)
  ), 
  file = paste0(p_output, "abn_dat_products", outfile_label, ".rds"))
}


# merge and clean the fragmentation results
cc_clean_frag_results <- function(site, n_metrics = 11, drop_0 = FALSE) {
  for (run in 1:n_metrics) {
    load(file = paste0(p_output, "frag/frag_", site, run, ".rds"),
         verbose = TRUE)
  }
  
  frag_l <- vector(mode = "list", length = n_metrics)
  names(frag_l) <- metrics_list[1:n_metrics]
  
  for (run in 1:n_metrics) {
    frag_l[[run]] <- eval(parse(text = paste0("frag_", site, run)))
  }
  
  frag <- bind_rows(frag_l)
  
  # ------------------- massage dataframe ------------------ #
  # Original Land-use class codes:
  #       1. Non-vegetated area (e.g. water, urban, barren land)
  #       2. Woody vegetation
  #       3. Cropland 
  #       4. Herbaceous land (e.g. grassland)
  
  frag <- frag %>% 
    mutate(land_cover = fct_recode(as_factor(class), 
                                   non_veg = "1", woody_veg = "2", 
                                   cropland = "3", grassland = "4"),
           year = layer + 1986,
           site = site)
  
  # if necessary, drop 0 land cover class
  if (drop_0) {
    frag <- frag %>%
      filter(land_cover %in% c("non_veg", "woody_veg", "cropland", "grassland", NA))
  }
  
  
  
  # ------------------- save cleaned df -------------------- #
  # assign name
  assign(paste0("frag_", site), frag)
  
  save(list = c(paste0("frag_", site)),
       file = paste0(p_output, "frag/frag_", site, ".rds")
  )
}



# plot and save functions ----

# ------------------------------------------------------------------------------------ #
# plot histograms
# ------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------ #
# plot lc and abandonment area over time
# ------------------------------------------------------------------------------------ #
cc_save_plot_lc_abn_area <- function(input_area_df, subtitle, outfile_label,
                                     width = 5, height = 4, save_all = FALSE) {
  
  # filter and recode
  if(save_all) {
    df <- input_area_df
  } else {
    df <- input_area_df %>% 
      filter(lc != "Abandoned (all)") %>% 
      mutate(lc = recode(lc, "Abandoned (>5)" = "Abandoned"))
  }
  
  gg_lc_abn_area <- ggplot(data = input_area_df) +
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
  png(filename = paste0(p_output, "plots/area_lc_abn", 
                        outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_lc_abn_area)
  dev.off()
}

# ------------------------------------------------------------------------------------ #
# plot persistence of abandonment over time
# ------------------------------------------------------------------------------------ #

cc_save_plot_abn_persistence <- function(input_list, subtitle, outfile_label,
                                         width = 7, height = 5,
                                         save_all = TRUE, subtitle_all = NULL) {
  
  # raw area
  gg_persistence_count <- ggplot(data = input_list$na_last) + 
    theme_classic() + 
    geom_line(mapping = aes(x = time_abn, y = area_ha / 10^3,
                            group = year_abn, color = year_abn), 
              size = 1.25) + 
    labs(y = expression("Area abandoned (10"^{3}*" ha)") , 
         x = "Years since initial abandonment", 
         title = "Persistence of Abandoned Land",
         subtitle = subtitle,
         color = "Year Abandoned") + 
    scale_color_distiller(palette = "Greens") + theme(legend.position = "bottom")
  
  # as percentage
  gg_persistence_proportion <- ggplot(data = input_list$na_last) + 
    theme_classic() + 
    geom_line(mapping = aes(x = time_abn, y = proportion,
                            group = year_abn, color = year_abn), 
              size = 1.25
    ) + 
    labs(y = "Proportion remaining abandoned", 
         x = "Years since initial abandonment", 
         title = "Persistence of Abandoned Land",
         subtitle = subtitle,
         color = "Year Abandoned") + 
    scale_color_distiller(palette = "Greens") + theme(legend.position = "bottom")
  
  # na_first ----------- #
  gg_persistence_count_na_first <- ggplot(data = input_list$na_first) + 
    theme_classic() + 
    geom_line(mapping = aes(x = year, y = area_ha / 10^3,
                            group = year_abn, color = year_abn), 
              size = 1.25
    ) + 
    labs(y = expression("Area abandoned (10"^{3}*" ha)") , 
         x = "Year", 
         title = "Persistence of Abandoned Land",
         subtitle = subtitle,
         color = "Year Abandoned") + 
    scale_color_distiller(palette = "Greens") + theme(legend.position = "bottom")
  
  gg_persistence_proportion_na_first <- ggplot(data = input_list$na_first) + 
    theme_classic() + 
    geom_line(mapping = aes(x = year, y = proportion,
                            group = year_abn, color = year_abn), 
              size = 1.25
    ) + 
    labs(y = "Proportion remaining abandoned", 
         x = "Year", 
         title = "Persistence of Abandoned Land",
         subtitle = subtitle,
         color = "Year Abandoned") + 
    scale_color_distiller(palette = "Greens") + theme(legend.position = "bottom")
  
  
  # save
  png(filename = paste0(p_output, "plots/persistence_", 
                        "count", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_count)
  dev.off()
  
  png(filename = paste0(p_output, "plots/persistence_", 
                        "proportion", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_proportion)
  dev.off()
  
  png(filename = paste0(p_output, "plots/persistence_", 
                        "count_na_first", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_count_na_first)
  dev.off()
  
  png(filename = paste0(p_output, "plots/persistence_", 
                        "proportion_na_first", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_proportion_na_first)
  dev.off()
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(p_output, "plots/persistence_", 
                          "count_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_count %+% input_list$na_last_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(p_output, "plots/persistence_", 
                          "proportion_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_proportion %+% input_list$na_last_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(p_output, "plots/persistence_", 
                          "count_na_first_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_count_na_first %+% input_list$na_first_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(p_output, "plots/persistence_", 
                          "proportion_na_first_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_proportion_na_first %+% input_list$na_first_all + 
            labs(subtitle = subtitle_all))
    dev.off()
  }
}


# ------------------------------------------------------------------------------------ #
# plot gains and losses of abandoned land, over time
# ------------------------------------------------------------------------------------ #
cc_save_plot_area_gain_loss <- function(input_area_change_df, subtitle, outfile_label,
                                        width = 6, height = 5,
                                        save_all = TRUE, subtitle_all = paste0(subtitle, ", all abandonment")) {
  
  # gain, loss, and net change in abandoned area, over time
  gg_abn_area_change_base <- ggplot() + 
    theme_classic() + 
    labs(y = expression("Change in area abandoned (10"^{3}*" ha)"), 
         x = "Year", 
         title = "Change in Area of Abandoned Land",
         subtitle = subtitle,
         fill = NULL, #"Change in Area",
         color = NULL) + 
    scale_color_manual(values = c("black"),
                       labels = "Net Change in Area") + 
    scale_fill_manual(values = brewer_pal(palette = "Greens")(5)[3:2],
                      labels = c("Area Gained", "Area Lost")) + 
    theme(legend.position = "bottom")
  
  gg_abn_area_change <- gg_abn_area_change_base +
    geom_col(data = filter(input_area_change_df, direction != "net"),
             mapping = aes(x = year, y = area_ha / (10^3), 
                           group = direction, fill = direction)) + 
    geom_line(data = filter(input_area_change_df, direction == "net"),
              mapping = aes(x = year, y = area_ha / (10^3), color = "Net Change in Area"),
              size = 1.5)
  
  gg_abn_area_change_all <- gg_abn_area_change_base +
    geom_col(data = filter(input_area_change_df, direction != "net"),
             mapping = aes(x = year, y = area_ha_all / (10^3), 
                           group = direction, fill = direction)) + 
    geom_line(data = filter(input_area_change_df, direction == "net"),
              mapping = aes(x = year, y = area_ha_all / (10^3), color = "Net Change in Area"),
              size = 1.5)
  
  
  # save to file
  png(filename = paste0(p_output, "plots/abn_area_change", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(gg_abn_area_change)
  dev.off()
  
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(p_output, "plots/abn_area_change_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(gg_abn_area_change_all)
    dev.off()
  }
  
}


# ------------------------------------------------------------------------------------ #
# plot area of abandonment, by age class
# ------------------------------------------------------------------------------------ #
cc_save_plot_area_by_age_class <- function(input_list, subtitle, outfile_label,
                                           width = 7, height = 5,
                                           save_all = TRUE) {
  
  age_class_base <- ggplot(data = input_list$na_first) + 
    theme_classic() +
    labs(y = expression("Area abandoned (10"^{3}*" ha)") , 
         x = "Year", 
         title = "Area of Abandoned Land, by Age Class",
         subtitle = subtitle,
         fill = "Age")
  
  age_class_continuous <- age_class_base +
    geom_col(mapping = aes(x = year, y = area_ha / 10^3, group = age, fill = age),
             position = position_stack(reverse = TRUE)
    ) + 
    scale_fill_distiller(palette = "Greens", direction = 1)
  
  
  age_class_bins <- age_class_base +
    geom_col(mapping = aes(x = year, y = area_ha / 10^3, group = bins, fill = bins),
             position = position_stack(reverse = TRUE)
    ) +
    scale_fill_brewer(palette = "Greens", direction = 1) 
  
  
  # save to file
  png(filename = paste0(p_output, "plots/abn_area_by_class_cont", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(age_class_continuous)
  dev.off()
  
  png(filename = paste0(p_output, "plots/abn_area_by_class_bins", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(age_class_bins)
  dev.off()
  
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(p_output, "plots/abn_area_by_class_cont_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(age_class_continuous %+% input_list$na_first_all)
    dev.off()
    
    png(filename = paste0(p_output, "plots/abn_area_by_class_bins_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(age_class_bins %+% input_list$na_first_all)
    dev.off()
  }
  
  
}


# ------------------------------------------------------------------------------------ #
# save four plot types, master function
# ------------------------------------------------------------------------------------ #

# save just the general intro plots
cc_save_area_persistence_plots <- function(input_site_label = outfile_label, 
                                 outfile_label,
                                 subtitle, 
                                 subtitle_all = paste0(subtitle, ", all abandonment"), 
                                 save_all = TRUE) {
  
  
  # ------------- calculate total area per lc, with abandonment ---------------- #
  cc_save_plot_lc_abn_area(input_area_df = eval(parse(text = paste0("area", input_site_label))), 
                           subtitle = subtitle, outfile_label = outfile_label,
                           save_all = save_all)
  
  
  # ------------------------ abandonment persistence --------------------------- #
  cc_save_plot_abn_persistence(input_list = eval(parse(text = paste0("persistence_list", input_site_label))), 
                               subtitle = subtitle, outfile_label = outfile_label,
                               save_all = save_all, subtitle_all = subtitle_all)
  
  
  # -------------------- calculate the abandonment area turnover ------------------- #
  cc_save_plot_area_gain_loss(input_area_change_df = eval(parse(text = paste0("abn_area_change", input_site_label))), 
                              subtitle = subtitle, outfile_label = outfile_label,
                              save_all = save_all,
                              subtitle_all = subtitle_all)
  
  
  # -------------------- plot abandonment area by age class ------------------- #
  cc_save_plot_area_by_age_class(input_list = eval(parse(text = paste0("persistence_list", input_site_label))), 
                                 subtitle = subtitle, outfile_label = outfile_label,
                                 save_all = save_all)
}


cc_save_frag_plots <- function(input = frag_dat, 
                               outfile_label) {
  
  # recode and filter land cover types
  input <- input %>%
    mutate(land_cover = fct_recode(as_factor(land_cover), 
                                   "Non-veg." = "non_veg",
                                   "Woody veg." = "woody_veg",
                                   "Cropland" = "cropland",
                                   "Grassland" = "grassland"),
           land_cover = fct_relevel(as_factor(land_cover), c("Non-veg.","Grassland", "Woody veg.", "Cropland")),
           site = fct_recode(as_factor(site), "Belarus" = "belarus", "Shaanxi" = "shaanxi")) %>%
    filter(land_cover != "Non-veg.")
  
  land_cover_cols <- c("Non-veg." = plot_cols$color[1], 
                       "Woody veg." = plot_cols$color[2], 
                       "Cropland" = plot_cols$color[3], 
                       "Grassland" = plot_cols$color[4])
  
  
  # make plotting base
  gg_frag_base <- ggplot(data = filter(input, metric == "ai")) +
    theme_classic() +
    geom_point(mapping = aes(x = year, y = value, color = land_cover)) + 
    geom_smooth(method = "lm", mapping = aes(x = year, y = value,
                                             fill = land_cover, 
                                             color = land_cover)) +
    labs(x = "Year", y = "Index Value", color = "Land Cover") + 
    guides(fill = FALSE) +
    facet_grid(cols = vars(site), scales = "free_y") +
    scale_colour_manual(
      values = land_cover_cols,
      aesthetics = c("colour", "fill")
    )
  
  # ------------------- 1. aggregation -------------------- #
  gg_frag_ai <- gg_frag_base %+% filter(input, metric == "ai") + 
    labs(title = "Aggregation Index, by land cover", y = "Aggregation Index Value")
  
  # ------------------- 2. clumpiness -------------------- #
  gg_frag_clumpy <- gg_frag_base %+% filter(input, metric == "clumpy") + 
    labs(title = "Clumpiness (Aggregation) Index, by land cover", y = "Clumpiness Index Value")
  
  
  # ------------------- aggregation combo -------------------- #
  
  gg_frag_aggregation_combo <- gg_frag_base %+% 
    filter(input, metric %in% c("ai", "clumpy")) +
    # labs(title = "Aggregation Indices, by land cover", y = "Index Value") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(ai = "Aggregation Index", 
                                              clumpy = "Clumpiness Index")))
  
  
  # ------------------- 3. Number of patches -------------------- #
  
  gg_frag_np <- ggplot(data = filter(input, metric == "np")) +
    theme_classic() +
    geom_point(mapping = aes(x = year, y = value/(10^3), color = land_cover)) + 
    geom_smooth(method = "lm", 
                mapping = aes(x = year, y = value/(10^3),
                              fill = land_cover, color = land_cover)) +
    labs(title = "Number of patches, by land cover",
         x = "Year", y = expression("Number of patches  (10"^{3}*")") , 
         color = "Land Cover") + 
    guides(fill = FALSE) +
    facet_grid(cols = vars(site)) + 
    scale_colour_manual(
      values = land_cover_cols,
      aesthetics = c("colour", "fill")
    )
  
  # ------------------- 4. Patch area, coefficient of variation -------------------- #
  gg_frag_patch_area_cv <- gg_frag_base %+% filter(input, metric == "area_cv") +
    labs(title = "Patch area coefficient of variation", y = "Patch Area CV (ha)")
  
  # ------------------- 5. Patch area, mean -------------------- #
  gg_frag_patch_area_mn <- gg_frag_base %+% filter(input, metric == "area_mn") +
    labs(title = "Mean patch area, by land cover", y = "Patch Area mean (ha)")
  
  # ------------------- 6. Patch area, sd -------------------- #
  
  gg_frag_patch_area_sd <- gg_frag_base %+% filter(input, metric == "area_sd") +
    labs(title = "Patch area standard deviation, by land cover", y = "Patch Area SD (ha)")
  
  # ------------------- patch area combo -------------------- #
  
  gg_frag_patch_area_combo <- gg_frag_base %+%
    filter(input, metric %in% c("area_cv", "area_mn", "area_sd")) +
    labs(title = "Patch Area, by land cover", y = "Patch Area (ha)") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(area_cv = "Coeff. Var.", 
                                              area_mn = "Mean",  
                                              area_sd = "Std. Dev.")))
  
  gg_frag_patch_area_variation <- gg_frag_base %+%
    filter(input, metric %in% c("area_cv", "area_sd")) +
    labs(title = "Variation in Patch Area, by land cover", y = "Patch Area (ha)") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(area_cv = "Coeffient of Variation (cv = sd/mean)", 
                                              area_mn = "Mean",  
                                              area_sd = "Standard Deviation (sd)")))
  
  
  # ------------------- 7. Total class area  -------------------- #
  
  gg_frag_ca <- ggplot(data = filter(input, metric == "ca")) +
    theme_classic() +
    geom_point(mapping = aes(x = year, y = value/(10^6), color = land_cover)) + 
    geom_line(mapping = aes(x = year, y = value/(10^6), color = land_cover)) + 
    labs(title = "Total area in each land cover type",
         x = "Year", y = expression("Class Area (10"^{6}*" ha)"), 
         color = "Land Cover") + 
    guides(fill = FALSE) +
    facet_grid(cols = vars(site), scales = "free_y") + 
    scale_colour_manual(
      values = land_cover_cols,
      aesthetics = c("colour", "fill")
    )
  
  
  
  # ------------------- 8. Total edge (meters) -------------------- #
  gg_frag_te <- ggplot(data = filter(input, metric == "te")) +
    theme_classic() +
    geom_point(mapping = aes(x = year, y = value/(10^6), color = land_cover)) + 
    geom_smooth(method = "lm", mapping = aes(x = year, y = value/(10^6),
                                             fill = land_cover,
                                             color = land_cover)) +
    labs(title = "Total edge in each land cover type",
         x = "Year", y = expression("Total edge (10"^{3}*" km)"), 
         color = "Land Cover") + 
    guides(fill = FALSE) +
    facet_grid(cols = vars(site), scales = "free_y") + 
    scale_colour_manual(
      values = land_cover_cols,
      aesthetics = c("colour", "fill")
    )
  
  
  # ------------------- 9-11. Perimeter-area ratio -------------------- #
  
  
  # ------------------- perimeter-area ratio combo -------------------- #
  gg_frag_para_combo <- gg_frag_base %+%
    filter(input, metric %in% c("para_cv", "para_mn", "para_sd")) +
    labs(title = "Perimeter-Area Ratio, by land cover", y = "Perimeter-Area Ratio") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(para_cv = "Coeffient of Variation (cv = sd/mean)", 
                                              para_mn = "Mean",  
                                              para_sd = "Standard Deviation (sd)")))
  
  gg_frag_para_variation <- gg_frag_base %+%
    filter(input, metric %in% c("para_cv", "para_sd")) +
    labs(title = "Variation in Perimeter-Area Ratio, by land cover", y = "Perimeter-Area Ratio") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(para_cv = "Coefficient of Variation (cv = sd/mean)", 
                                              para_mn = "Mean",  
                                              para_sd = "Standard Deviation (sd)")))
  
  
  gg_frag_para_cv <- gg_frag_base %+% filter(input, metric == "para_cv") +
    labs(title = "Perimeter-Area Ratio, by land cover", y = "Perimeter-Area Ratio (cv)")
  
  gg_frag_para_mn <- gg_frag_base %+% filter(input, metric == "para_mn") +
    labs(title = "Perimeter-Area Ratio, by land cover", y = "Perimeter-Area Ratio (mean)")
  
  gg_frag_para_sd <- gg_frag_base %+% filter(input, metric == "para_sd") +
    labs(title = "Perimeter-Area Ratio, by land cover", y = "Perimeter-Area Ratio (sd)")
  
  
  
  
  # save to png the main plots
  # a. aggregation combo
  # b. number of patches
  # c. patch area combo
  # d. total edge
  # e. perimeter-area ratio combo
  
  png(filename = paste0(p_output, "plots/frag_aggregation_combo", outfile_label, ".png"), 
      width = 7, height = 5, units = "in", res = 400)
  print(gg_frag_aggregation_combo)
  dev.off()
  
  
  png(filename = paste0(p_output, "plots/frag_number_patches", outfile_label, ".png"), 
      width = 7, height = 4, units = "in", res = 400)
  print(gg_frag_np)
  dev.off()
  
  
  png(filename = paste0(p_output, "plots/frag_patch_area_combo", outfile_label, ".png"), 
      width = 7, height = 8, units = "in", res = 400)
  print(gg_frag_patch_area_combo)
  dev.off()
  
  
  png(filename = paste0(p_output, "plots/frag_total_edge", outfile_label, ".png"), 
      width = 7, height = 4, units = "in", res = 400)
  print(gg_frag_te)
  dev.off()
  
  
  png(filename = paste0(p_output, "plots/frag_pa_ratio_combo", outfile_label, ".png"), 
      width = 7, height = 8, units = "in", res = 400)
  print(gg_frag_para_combo)
  dev.off()
  
  # extras:
  png(filename = paste0(p_output, "plots/frag_clumpy", outfile_label, ".png"), 
      width = 7, height = 4, units = "in", res = 400)
  print(gg_frag_clumpy)
  dev.off()
  
  # number of patches and mean patch area
  png(filename = paste0(p_output, "plots/frag_patch_area_num", outfile_label, ".png"), 
      width = 7, height = 6, units = "in", res = 400)
  print(plot_grid(gg_frag_patch_area_mn, gg_frag_np, nrow = 2))
  dev.off()
  
  
  # patch area variation
  png(filename = paste0(p_output, "plots/frag_patch_area_var", outfile_label, ".png"), 
      width = 7, height = 6, units = "in", res = 400)
  print(gg_frag_patch_area_variation)
  dev.off()
  
  
  # total edge and the perimeter-area ratio 
  png(filename = paste0(p_output, "plots/frag_para-te", outfile_label, ".png"), 
      width = 7, height = 6, units = "in", res = 400)
  print(plot_grid(gg_frag_para_mn, gg_frag_te, nrow = 2))
  dev.off()
  
  png(filename = paste0(p_output, "plots/frag_para_var", outfile_label, ".png"), 
      width = 7, height = 6, units = "in", res = 400)
  print(gg_frag_para_variation)
  dev.off()
  
  
}

# save raster functions ---- 

# ------------------------------------------------------------------------------------ #
# plot map: pnv, habitats, land cover, abandonment
# ------------------------------------------------------------------------------------ #
cc_save_map_pnv_hab_lc_abn <- function(maxpixels, width = 9, height = 5.5) {
  
  pdf(file = paste0(p_output, "plots/pnv-habitat-lc-age.pdf"),
      width = 9, height = 5.5)
  
  # Set plot layout
  # layout(mat = matrix(1:8, nrow = 2, ncol = 4),
  #        heights = c(1.5, 1.5),    # Heights of the two rows
  #        widths = c(2, 2, 2, 2))     # Widths of the two columns
  # layout.show(8)
  
  # layout(mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  #        heights = c(1.5, 1.5),    # Heights of the two rows
  #        widths = c(2, 2, 2, 2))     # Widths of the two columns
  # layout.show(8)
  
  par(mfrow = c(2, 4),
      oma = c(0,0,0,1))
  
  # --------------------------------------- #
  # ------------ Shaanxi ------------------ #
  # --------------------------------------- #
  
  # ------------- potential natural vegetation ---------------- #
  plot(s_pnv, maxpixels = 100000, main = "Shaanxi, PNV",
       breaks = c(0, filter(pnv_table, Number %in% unique(values(s_pnv)))$Number),
       col = filter(pnv_table, Number %in% unique(values(s_pnv)))$Color)
  
  legend("topleft", 
         legend = filter(pnv_table, Number %in% unique(values(s_pnv)))$Name, 
         fill = filter(pnv_table, Number %in% unique(values(s_pnv)))$Color,
         cex = 0.6, inset = 0)
  
  
  # ------------- habitat types ---------------- #
  plot(s_habitat, main = "Shaanxi, Habitat Types",
       breaks = c(-1, 
                  filter(habitat_table, Number %in% unique(values(s_habitat)))$Number),
       col = filter(habitat_table, Number %in% unique(values(s_habitat)))$Color,
       maxpixels = maxpixels)
  
  legend("topleft", 
         legend = filter(habitat_table, Number %in% unique(values(s_habitat)))$Name, 
         fill = filter(habitat_table, Number %in% unique(values(s_habitat)))$Color,
         cex = 0.6, inset = 0)
  
  # ------------- raw land use data ---------------- #
  plot(s$y2015, main = "Shaanxi 2015", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color,
       maxpixels = maxpixels)
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  # ------------- abandonment age ---------------- #
  plot(s_age_r$y2017, main = "Shaanxi, 2017: \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  
  # --------------------------------------- #
  # ------------ Belarus ------------------ #
  # --------------------------------------- #
  
  # ------------- potential natural vegetation ---------------- #
  plot(b_pnv, maxpixels = 100000, main = "Belarus, PNV",
       breaks = c(0, filter(pnv_table, Number %in% unique(values(b_pnv)))$Number),
       col = filter(pnv_table, Number %in% unique(values(b_pnv)))$Color)
  
  legend("bottomleft", 
         legend = filter(pnv_table, Number %in% unique(values(b_pnv)))$Name, 
         fill = filter(pnv_table, Number %in% unique(values(b_pnv)))$Color,
         cex = 0.6, inset = 0)
  
  # ------------- habitat types ---------------- #
  plot(b_habitat, main = "Belarus, Habitat Types",
       breaks = c(-1, 
                  filter(habitat_table, Number %in% unique(values(b_habitat)))$Number),
       col = filter(habitat_table, Number %in% unique(values(b_habitat)))$Color,
       maxpixels = maxpixels)
  
  legend("bottomleft", 
         legend = filter(habitat_table, Number %in% unique(values(b_habitat)))$Name, 
         fill = filter(habitat_table, Number %in% unique(values(b_habitat)))$Color,
         cex = 0.6, inset = 0)
  
  
  # ------------- raw land use data ---------------- #
  plot(b$y2015, main = "Belarus 2015", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color,
       maxpixels = maxpixels)
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  
  # ------------- abandonment age ---------------- #
  plot(b_age_r$y2017, main = "Belarus, 2017: \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  
  dev.off()
}



# ------------------------------------------------------------------------------------ #
# plot map: land cover in 1987, 2017, age of abandonment in 2017, and max age
# ------------------------------------------------------------------------------------ #
cc_save_map_lc_age_rasters <- function(maxpixels, width = 9, height = 5.5) {
  pdf(file = paste0(p_output, "plots/rasters_lc_w_abn_age.pdf"),
      width = width, height = height)
  
  # Set plot layout
  par(mfrow = c(2, 4),
      oma = c(0,0,0,1))
  
  # left to right
  # LC 1987 
  # LC 2017
  # Time abandoned 2017
  # Max length abandoned
  
  # row 1
  # --------------------------------------- #
  # ------------ Shaanxi ------------------ #
  # --------------------------------------- #
  
  # ------------- raw land use data ---------------- #
  plot(s$y1987, main = "Shaanxi 1987", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color, maxpixels = maxpixels
  )
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  plot(s$y2017, main = "Shaanxi 2017", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color,
       maxpixels = maxpixels)
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  # ------------- abandonment age ---------------- #
  plot(s_age_r$y2017, main = "Shaanxi, 2017: \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  # ------------- max abandonment length (age) ---------------- #
  plot(s_max_length_r, main = "Shaanxi: Maximum \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  # row two
  # --------------------------------------- #
  # ------------ Belarus ------------------ #
  # --------------------------------------- #
  
  
  # ------------- raw land use data ---------------- #
  plot(b$y1987, main = "Belarus 1987", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color,
       maxpixels = maxpixels)
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  plot(b$y2017, main = "Belarus 2017", 
       breaks = c(0, plot_cols$breaks), col = plot_cols$color,
       maxpixels = maxpixels)
  legend("bottomleft", cex = 0.6, inset = 0,
         legend = plot_cols$name, 
         fill = plot_cols$color)
  
  
  # ------------- abandonment age ---------------- #
  plot(b_age_r$y2017, main = "Belarus, 2017: \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  # ------------- max abandonment length (age) ---------------- #
  plot(b_max_length_r, main = "Belarus: Maximum \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  dev.off()
}





