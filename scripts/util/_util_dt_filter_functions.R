# --------------------------------------------------------------- #
#
# abandonment trajectories custom functions
# 
# --------------------------------------------------------------- #


# raster prep functions ----

# -------------------------------------------------------------------------- #
# Load individual raster layers and merge into a single raster brick
# -------------------------------------------------------------------------- #
cc_merge_rasters <- function(site, site_df,
                             input_path){
  cat(fill = TRUE, "cc_merge_rasters: Merging raw raster layers into a single stack for ", site)
  
  site_files <- list.files(paste0(input_path, site), full.names = TRUE)
  
  if (site_df[site_df$site == site, "merge_layers"] == "Yes") {
    
    # load raster layers
    raster_layers_list <- lapply(site_files, function(i) raster(i))
    
    # create raster stack
    tic("create raster stack")
    raster_stack <- stack(raster_layers_list)
    toc(log = TRUE)
    
  } else {
    
    # load raster stack
    tic("load raster stack")
    raster_stack <- brick(site_files)
    toc(log = TRUE)
    
  }
  
  # "print and save names of the raster stack"
  cat(fill = TRUE, "raster_stack layer names:")
  print(names(raster_stack))
  fwrite(data.frame(names(raster_stack)), file = paste0(input_path, site, "_raw_layer_names.csv"))
  
  # write to file
  tic("write raster_stack to file")
  writeRaster(raster_stack, 
              filename = paste0(input_path, site, "_raw.tif"),
              overwrite = TRUE)
  toc(log = TRUE)
  
  cat(fill = TRUE, "Done! Used cc_merge_rasters() to merge and save raw raster brick for: ", site)

}




# -------------------------------------------------------------------------- #
# Recode land cover classes in raw rasters, writing to data_derived
# -------------------------------------------------------------------------- #
# note, this does not work, as of 11/10/20. Recode directly in data.tables instead.
cc_recode_rasters <- function(site, site_df, input_path, 
                              output_path){
  print(paste0("cc_recode_rasters: Recoding land cover classes for ", site))
  
  tic.clearlog()
  
  tic("load input raster")
  input_raster <- brick(paste0(input_path, site, "_raw.tif"))
  toc(log = TRUE)
  
  print("freq(input_raster[[1]]): before")
  print(freq(input_raster[[1]]))
  
  if (site_df[site_df$site == site, "update_lc"] == "Yes") {
    # recode the raw input raster, if necessary:
    
    tic("add 10 to input raster")
    input_raster <- input_raster + 10 # add 10 to the input raster, to facilitate recoding
    toc(log = TRUE)
    # ^ without this, we risk lumping land cover classes together 
    # based on the order in which things are recoded
    
    original_codes <- site_df[site_df$site == site, ]
    print(original_codes)
    
    tic("recode land cover classes: 10 -> NA")
    input_raster[input_raster == 10] <- NA # remove 0s
    toc(log = TRUE)
    
    tic("recode land cover classes: other -> 1")
    print(paste0("original other code: ", original_codes[, "other"]))
    input_raster[input_raster == original_codes[, "other"] + 10] <- 1 # set other to 1
    toc(log = TRUE)
    
    tic("recode land cover classes: woody_veg -> 2")
    print(paste0("original woody_veg code: ", original_codes[, "woody_veg"]))
    input_raster[input_raster == original_codes[, "woody_veg"] + 10] <- 2 # set woody_veg to 2
    toc(log = TRUE)
    
    tic("recode land cover classes: cropland -> 3")
    print(paste0("original cropland code: ", original_codes[, "cropland"]))
    input_raster[input_raster == original_codes[, "cropland"] + 10] <- 3 # set cropland to 3
    toc(log = TRUE)
    
    tic("recode land cover classes: grassland -> 4")
    print(paste0("original grassland code: ", original_codes[, "grassland"]))
    input_raster[input_raster == original_codes[, "grassland"] + 10] <- 4 # set grassland to 4
    toc(log = TRUE)
  } 
  
  print("freq(input_raster[[1]]): after")
  print(freq(input_raster[[1]]))
  
  # write to file
  tic("write recoded raster to file")
  writeRaster(input_raster, 
              filename = paste0(output_path, site, ".tif"),
              overwrite = TRUE)
  toc(log = TRUE)
  
  print(paste0("Done. Raster land cover recoded for: ", site))
  
  # print("Here's the tic.log:")
  # print(tic.log())
  
}



# data.table filtering functions ----
# -------------------------------------------------------------------------- #
# Update land cover classes values to lump grassland and woody vegetation into 
# a single "noncrop" category, and make binary
# -------------------------------------------------------------------------- #
cc_update_lc <- function(dt, crop_code = 0, noncrop_code = 1) {
  
  # Original Land-use class codes:
  #       1. Non-vegetated area (e.g. water, urban, barren land)
  #       2. Woody vegetation
  #       3. Cropland 
  #       4. Herbaceous land (e.g. grassland)
  
  # this function updates these land cover classes to the codes provided, 
  # which are be default:
  #       0. for crop
  #       1. for noncrop (woody vegetation and grassland)
  
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
    for (i in seq_len(length(dt))) { # iterating across columns
      dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1] 
    }
  }
}

# alternatively: dt <- dt - 1 works just fine




# -------------------------------------------------------------------------- #
# Filter out pixels that are either all crop or all noncrop
# -------------------------------------------------------------------------- #
cc_remove_non_abn <- function(dt) {
  # this function filters out all rows for which 
  dt[dt[, rowSums(.SD) > 0 & rowSums(.SD) < length(.SD), 
        .SDcols = grep("[xy]$", names(dt), invert = TRUE)], ] 
  
  # must be used in the format:
  # dt <- cc_remove_non_abn(dt) 
  # this is inefficient with memory, but since I'll be in adroit anyways, it'll probably be fine.
  
  # all crop row sums = 0
  # all noncrop row sums = length(dt) (i.e. the number of columns)
}



# -------------------------------------------------------------------------- #
# Count number of periods of recultivation that fall below a continuous recultivation threshold.
# -------------------------------------------------------------------------- #
cc_count_blips <- function(dt, start_year = 1987) {
  # use as follows:
  # df <- cc_count_blips(dt)
  # This function counts the number of short recultivation periods that fall
  # below a specified threshold of continuous recultivation, in each year.
  
  # check that dt starts with x & y
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } else {
      start <- 4
    }
  } else {
    start <- 2
  }
  
  # 1-0-1 blips
  blips_1 <- sapply(start:(ncol(dt) - 1), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 1, 
       .N]
  }
  )

  # 1-0-0-1 blips
  blips_2 <- sapply(start:(ncol(dt) - 2), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 1, 
       .N]
  }
  )
  
  # 1-0-0-0-1 blips
  blips_3 <- sapply(start:(ncol(dt) - 3), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 0 & 
         get(names(dt)[i+3]) == 1, .N]
  }
  )

  blips_df <- data.frame(
    year = 1:length(grep("x$|y$", invert = T, names(dt))) - 1 + start_year,
    one = c(0, blips_1, rep.int(0, 1)),
    two = c(0, blips_2, rep.int(0, 2)),
    three = c(0, blips_3, rep.int(0, 3)))
  
  blips_df
}


# --------------------- count blips of just one individual type ---------------------- #
cc_count_blips_ind <- function(dt, 
                               recultivation_threshold = 1,
                               return_df = TRUE,
                               start_year = 1987
) {
  # use as follows:
  # cc_count_blips(dt)
  # This function counts the number of short recultivation periods that fall
  # below a specified threshold of continuous recultivation, in each year.
  
  # error if incompatible threshold
  stopifnot("Function currently only accepts recultivation thresholds of 1 through 3." =
              {recultivation_threshold >= 1
                recultivation_threshold <= 3
              })
  
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } else {
      start <- 4
    }
  } else {
    start <- 2
  }
  
  # 1-0-1
  if(recultivation_threshold == 1) {
  blips <- sapply(start:(ncol(dt) - recultivation_threshold), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 1, 
       .N]
  }
  )
  }
  
  # 1-0-0-1 blips
  if(recultivation_threshold == 2) {
  blips <- sapply(start:(ncol(dt) - recultivation_threshold), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 1, 
       .N]
  }
  )
  }
  
  # 1-0-0-0-1 blips
  if(recultivation_threshold == 3) {
  blips <- sapply(start:(ncol(dt) - recultivation_threshold), function(i) {
    dt[get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 0 & 
         get(names(dt)[i+3]) == 1, .N]
  }
  )
  }
  
  blips <- c(0, blips, rep.int(0, recultivation_threshold))
  blips_df <- data.frame(
    year = seq_along(dt) - 1 + start_year,
    count = blips
    )
  
  ifelse(return_df, 
         return(blips_df), 
         return(blips))
}

# -------------------------------------------------------------------------- #
# Fill short periods of recultivation that fall below a continuous recultivation threshold.
# -------------------------------------------------------------------------- #
cc_fill_blips <- function(dt, recultivation_threshold = 1, replacement_value = 1) {
  # notes:
  # This function fills short periods of recultivation that fall below a recultivation threshold.
  # It recodes from crop back to non-crop, based on how many continuous years 
  # of crop classification are required by the threshold.
  # This cleaning step should occur before calculating the age.
  # Use as follows:
  # cc_fill_blips(dt)
  
  stopifnot("Function currently only accepts recultivation thresholds of 1 through 3." = 
              {recultivation_threshold >= 1
                recultivation_threshold <= 3
              })
  
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } else {
      start <- 4
    }
  } else {
    start <- 2
  }
  
  # 1-0-1
  if(recultivation_threshold == 1) {
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 1, # subset to 1-0-1
         
         # then, set the value in those columns to 1 (plugging the hole)
         names(dt)[i] := replacement_value]
    }
  }
  
  # 1-0-0-1 blips
  if(recultivation_threshold == 2) {
    # fill 1-0-1 first, 
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 1, # subset to 1-0-1
         
         # then, set the value in those columns to 1 (plugging the hole)
         names(dt)[i] := replacement_value]
    }
    
    # then fill 1-0-0-1
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 0 & 
           get(names(dt)[i+2]) == 1, # subset to 1-0-0-1
         
         # then, set the value in those columns to 1 (plugging the hole)
         c(names(dt)[i], 
           names(dt)[i+1]) := replacement_value]
    }
  }
  
  
  # 1-0-0-0-1 blips
  if(recultivation_threshold == 3) {
    # fill 1-0-1 first, 
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 1, # subset to 1-0-1
         
         # then, set the value in those columns to 1 (plugging the hole)
         names(dt)[i] := replacement_value]
    }
    
    # then fill 1-0-0-1
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 0 & 
           get(names(dt)[i+2]) == 1, # subset to 1-0-0-1
         
         # then, set the value in those columns to 1 (plugging the hole)
         c(names(dt)[i], 
           names(dt)[i+1]) := replacement_value]
    }
    
    # then fill 1-0-0-0-1
    for (i in start:(ncol(dt) - recultivation_threshold)) {
      dt[get(names(dt)[i-1]) == 1 & 
           get(names(dt)[i]) == 0 & 
           get(names(dt)[i+1]) == 0 & 
           get(names(dt)[i+2]) == 0 & 
           get(names(dt)[i+3]) == 1, # subset to 1-0-0-0-1
         
         # then, set the value in that column to 1 (plugging the hole)
         c(names(dt)[i], 
           names(dt)[i+1],
           names(dt)[i+2]) := replacement_value]
    }
  }
}



# -------------------------------------------------------------------------- #
# Count the number of pixels affected by the 5- and 8-year moving window temporal filters
# -------------------------------------------------------------------------- #
cc_temporal_filter_count <- function(dt) {
  
  # check that dt starts with x & y
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } else {
      start <- 4
    }
  } else {
    start <- 2
  }
  
  # ----------------------------------------- #
  # for each column, return the row indices that match the following pattern, iterating across all columns
  affected_rows_5_l <- lapply(5:(ncol(dt) - 2), function(i) {
    dt[get(names(dt)[i-2]) == 1 & 
         get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 1 & 
         get(names(dt)[i+2]) == 1, which = TRUE] # which = TRUE returns the row indices
  }
  )
  
  # then condense the list into a single vector and remove duplicates
  affected_rows_5 <- affected_rows_5_l %>% unlist() %>% unique() 
  
  # for each column, determine the number of number of rows that match the following pattern:
  affected_rows_5_count_by_year <- sapply(affected_rows_5_l, function(i) length(i)) 

  # ----------------------------------------- #
  # do the same, but for the eight year moving window filter, counting from the first 0 position
  affected_rows_8_l <- lapply(6:(ncol(dt) - 4), function(i) {
    dt[get(names(dt)[i-3]) == 1 & # index here has to start at 3
         get(names(dt)[i-2]) == 1 & 
         get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 1 & 
         get(names(dt)[i+3]) == 1 & 
         get(names(dt)[i+4]) == 1, which = TRUE]
  }
  )
  
  # then condense the list into a single vector and remove duplicates
  affected_rows_8 <- affected_rows_8_l %>% unlist() %>% unique() 
  
  # for each column, determine the number of number of rows that match the following pattern:
  affected_rows_8_count_by_year <- sapply(affected_rows_8_l, function(i) length(i)) 
  
  # -------------------------------------------------------------------------- #
  # Count edge cases of 5- and 8-year moving window temporal filters
  # -------------------------------------------------------------------------- #
  
  # return row indices that match specific edge cases for each temporal filter
  # ---------------------------------------------------------- #
  # five year filter, edge cases
  
  # ------ start ------ #
  edge5_start_1 <- 
    dt[y1987 == 0 & 
         y1988 == 1 & 
         y1989 == 1, which = TRUE]
  
  edge5_start_2 <- 
    dt[y1987 == 1 & 
         y1988 == 0 & 
         y1989 == 1 &
         y1990 == 1, which = TRUE]
  
  # ------ end ------ #
  edge5_end_1 <- 
    dt[y2015 == 1 & 
         y2016 == 1 & 
         y2017 == 0, which = TRUE]
  
  edge5_end_2 <- 
    dt[y2014 == 1 & 
         y2015 == 1 & 
         y2016 == 0 & 
         y2017 == 1, which = TRUE]
  
  # ---------------------------------------------------------- #
  # eight year filter, edge cases
  
  # ------ start ------ #
  edge8_start_1 <- 
    dt[y1987 == 0 & 
         y1988 == 1 & 
         y1989 == 1 &
         y1990 == 1, which = TRUE]
  
  edge8_start_2 <- 
    dt[y1987 == 0 & 
         y1988 == 0 & 
         y1989 == 1 & 
         y1990 == 1 &
         y1991 == 1, which = TRUE]
  
  edge8_start_3 <- 
    dt[y1987 == 1 & 
         y1988 == 0 & 
         y1989 == 0 & 
         y1990 == 1 &
         y1991 == 1 &
         y1992 == 1, which = TRUE]
  
  edge8_start_4 <- 
    dt[y1987 == 1 & 
         y1988 == 1 & 
         y1989 == 0 & 
         y1990 == 0 &
         y1991 == 1 &
         y1992 == 1 & 
         y1993 == 1, which = TRUE]
  
  # ------ end ------ #
  edge8_end_1 <- 
    dt[y2014 == 1 & 
         y2015 == 1 & 
         y2016 == 1 & 
         y2017 == 0, which = TRUE]
  
  edge8_end_2 <- 
    dt[y2013 == 1 & 
         y2014 == 1 & 
         y2015 == 1 & 
         y2016 == 0 & 
         y2017 == 0, which = TRUE]
  
  edge8_end_3 <- 
    dt[y2012 == 1 & 
         y2013 == 1 & 
         y2014 == 1 & 
         y2015 == 0 & 
         y2016 == 0 & 
         y2017 == 1, which = TRUE]
  
  edge8_end_4 <- 
    dt[y2011 == 1 & 
         y2012 == 1 & 
         y2013 == 1 & 
         y2014 == 0 & 
         y2015 == 0 & 
         y2016 == 1 & 
         y2017 == 1, which = TRUE]
  
  # ---------------------------------------------------------- #
  # make a list containing the indices that match the edge cases:
  edge_cases_l <- list(edge5_start_1 = edge5_start_1, 
                       edge5_start_2 = edge5_start_2,
                       edge8_start_1 = edge8_start_1, 
                       edge8_start_2 = edge8_start_2, 
                       edge8_start_3 = edge8_start_3, 
                       edge8_start_4 = edge8_start_4,
                       edge5_end_1 = edge5_end_1, 
                       edge5_end_2 = edge5_end_2,
                       edge8_end_1 = edge8_end_1, 
                       edge8_end_2 = edge8_end_2, 
                       edge8_end_3 = edge8_end_3, 
                       edge8_end_4 = edge8_end_4)
  
  # ---------------------------------------------------------- #
  # construct data.frames
  
  # non-edge cases:
  cases_df <- data.frame(
    year = 1:length(grep("x$|y$", invert = T, names(dt))) - 1 + 1987,
    five_yr = c(rep.int(0, 2), affected_rows_5_count_by_year, rep.int(0, 2)),
    eight_yr = c(rep.int(0, 3), affected_rows_8_count_by_year, rep.int(0, 4)))
  
  nrow_affected <- data.frame(
    filter = c("five_yr", "eight_yr", "either"),
    nrow_affected = c(length(affected_rows_5),
                      length(affected_rows_8),
                      length(unique(c(affected_rows_5, affected_rows_8)))
    )
  )
  
  edge_cases_df <- data.frame(
    filter = rep(c(rep("five", 2), rep("eight", 4)), 2),
    edge = c(rep("start", 6), rep("end", 6)),
    pattern = c(
      c("|011", "|1011", 
        "|0111", "|00111", "|100111", "|1100111"),
      c("110|","1101|", 
        "1110|", "11100|", "111001|", "1110011|")),
    type = "cases",
    value = sapply(edge_cases_l, function(x) {length(x)})) # calculate the number of pixels affected by each edge case
  
  edge_nrow_affected <- data.frame(
    filter = "both",
    edge = c("start", "end", "either"),
    type = "pixels_affected",
    value = c(edge_cases_l[grep("start", names(edge_cases_l))] %>% unlist %>% unique %>% length, # count the pixels affected by any of the edge cases at the start
              edge_cases_l[grep("end", names(edge_cases_l))] %>% unlist %>% unique %>% length,
              edge_cases_l %>% unlist %>% unique %>% length # count pixels affected by either filter, either end)
    )) %>%
    mutate(percent_affected = value/nrow(dt))
  
  # ---------------------------------------------------------- #
  # return the various lists and data.frames bound together:
  # return the lists
  list(
    # lists
    affected_rows_5 = affected_rows_5, 
    affected_rows_8 = affected_rows_8,
    affected_rows_edge_l = edge_cases_l[1:8], 
    
    # dfs
    cases_df = cases_df,
    edge_cases_df = edge_cases_df, 
    nrow_affected = nrow_affected,
    edge_nrow_affected = edge_nrow_affected)
}


# -------------------------------------------------------------------------- #
# Temporal filters, 5-year and 8-year moving window filters (added February 19th, 2021)
# -------------------------------------------------------------------------- #
cc_temporal_filter <- function(dt, replacement_value = 1, filter_edge = TRUE) {
  # Five- and eight-year moving window filters designed to address potential misclassification errors
  # in the Yin et al. 2018 time series data.
  
  # check that dt starts with x & y
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    } else {
      start <- 4
    }
  } else {
    start <- 2
  }
  
  # default filters:
  # ---------------------------------------------------------- #
  # five year moving window: 
  # fill 1-1-0-1-1
  for (i in 5:(ncol(dt) - 2)) {
    dt[get(names(dt)[i-2]) == 1 &  # subset to 1-1-0-1-1
         get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 1 & 
         get(names(dt)[i+2]) == 1,
       
       names(dt)[i] := replacement_value # update value
    ]
  }
  
  # ---------------------------------------------------------- #
  # eight year moving window filter:
  # fill 1-1-1-0-0-1-1-1
  for (i in 6:(ncol(dt) - 4)) { # index here has to start at 3
    dt[get(names(dt)[i-3]) == 1 & # subset to 1-1-1-0-0-1-1-1
         get(names(dt)[i-2]) == 1 & 
         get(names(dt)[i-1]) == 1 & 
         get(names(dt)[i]) == 0 & 
         get(names(dt)[i+1]) == 0 & 
         get(names(dt)[i+2]) == 1 & 
         get(names(dt)[i+3]) == 1 & 
         get(names(dt)[i+4]) == 1, 
       
       c(names(dt)[i], 
         names(dt)[i+1]) := replacement_value # update value
    ]
  }
  
  
  # -------------------------------------------------------------------------- #
  # Temporal filter, edge cases (start only)
  # -------------------------------------------------------------------------- #
  if(filter_edge) {
    # note: this filter only addresses edge cases at the start of the time series.
    
    # ---------------------------------------------------------- #
    # five year moving window
    dt[y1987 == 0 & 
         y1988 == 1 & 
         y1989 == 1, 
       c("y1987") := replacement_value]
    
    dt[y1987 == 1 & 
         y1988 == 0 & 
         y1989 == 1 &
         y1990 == 1, 
       c("y1988") := replacement_value]
    
    # ---------------------------------------------------------- #
    # eight year filter, edge cases
    dt[y1987 == 0 & 
         y1988 == 1 & 
         y1989 == 1 &
         y1990 == 1, 
       c("y1987") := replacement_value]
    
    dt[y1987 == 0 & 
         y1988 == 0 & 
         y1989 == 1 & 
         y1990 == 1 &
         y1991 == 1, 
       c("y1987", "y1988") := replacement_value]
    
    dt[y1987 == 1 & 
         y1988 == 0 & 
         y1989 == 0 & 
         y1990 == 1 &
         y1991 == 1 &
         y1992 == 1, 
       c("y1988", "y1989") := replacement_value]
    
    dt[y1987 == 1 & 
         y1988 == 1 & 
         y1989 == 0 & 
         y1990 == 0 &
         y1991 == 1 &
         y1992 == 1 & 
         y1993 == 1, 
       c("y1989", "y1990") := replacement_value]
  }
  
  dt
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
# This removes age values for noncrop vegetation that start the time-series as noncrop - 
# this can't be classified as abandonment, since we don't know what came before the time-series, but we
# also can't just exclude the entire pixel, since there may be abandonment following a period of cropland.

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
# Recode land cover classes for each site, using data.tables, then  to calculate and extract abandonment periods
# -------------------------------------------------------------------------- #
cc_recode_lc_dt <- function(dt, site, site_df) {
  # Update data.table land use class codes to match those used by He:
  #       1. Non-vegetated area (e.g. water, urban, barren land)
  #       2. Woody vegetation
  #       3. Cropland 
  #       4. Herbaceous land (e.g. grassland)
  
  # Shaanxi (China):      1 others;     2 forest,     3 cropland;     4 grassland
  # Belarus / Russia:     1 others;     2 forest,     3 cropland;     4 grassland
  # Goias (Brazil):       1 others;     2 forest,     3 cropland;     4 grassland
  # Mato Grosso (Brazil): 1 others;     2 forest;     3 cropland;     4 grassland
  # Bosnia & Herzegovina: 1 others;     2 forest;     3 cropland;     4 grassland
  # Chongqing (China):    1 others;     2 forest;     3 cropland;     4 grassland
  
  # Iraq:                 1 others;     2 cropland;   3 forest;       4 grassland
  # Nebraska (US):        1 cropland;   2 forest;     3 others;       4 grassland
  # Orenburg (Russia):    1 others;     2 cropland;   3 grassland;    4 forest
  # Volgograd (Russia):   1 others;     2 cropland;   3 grassland;    4 forest
  # Wisconsin (US):       1 cropland;   2 grassland;  3 forest;       4 others

  
  # # make sure site_df is a normal data.frame, otherwise the selection below won't work correctly.
  # site_df <- as.data.frame(site_df) 
  
  cat(fill = TRUE, "cc_recode_lc_dt: recoding land cover classes (only for the following sites:", 
      site_df[site_df$update_lc == "Yes", "site"], ")")
  
  if (site_df[site_df$site == site, "update_lc"] == "Yes") {
    # recode the raw input raster, if necessary:
    
    # calculate the frequency of each land cover class
    cat(fill = TRUE, "frequency of each land cover class, before update")
    print(dt[, .N, by = y1987][order(y1987)]) # get freq
    
    # add 10 to the data.table
    tic("add 10 to input data.table")
    dt <- cbind(dt[, 1:2], dt[, 3:length(dt)] + 10)
    toc()
    # ^ without this, we risk lumping land cover classes together 
    # based on the order in which things are recoded
    
    # print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    
    # extract original land cover classifications
    original_codes <- site_df[site_df$site == site, ]
    print(t(original_codes))

    
    # set 0 values to NA
    tic("recode land cover classes: 10 -> NA")
    for (column in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[column]] == 10), j = column, value = NA)       
    }
    toc(log = TRUE)
    # print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    
    
    # set other (nonveg, urban, water, etc.) to 1
    cat(fill = TRUE, "original other code: ", original_codes$other)
    
    tic("recode other -> 1")
    for (column in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[column]] == original_codes[, "other"] + 10), j = column, value = 1)       
    }
    toc(log = TRUE)
    # print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    

        
    # set woody_veg to 2
    cat(fill = TRUE, "original woody_veg code: ", original_codes$woody_veg)
    tic("recode woody_veg -> 2")
    for (column in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[column]] == original_codes$woody_veg + 10), j = column, value = 2)      
    }
    toc(log = TRUE)
    # print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    
    
    
    # set cropland to 3
    cat(fill = TRUE, "original cropland code: ", original_codes$cropland)
    tic("recode cropland -> 3")
    for (column in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[column]] == original_codes$cropland + 10), j = column, value = 3)      
    }
    toc(log = TRUE)
    # print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    
    
    
    # set grassland to 4
    cat(fill = TRUE, "original grassland code: ", original_codes$grassland)
    tic("recode grassland -> 4")
    for (column in names(dt[, 3:length(dt)])) {
      set(dt, i = which(dt[[column]] == original_codes$grassland + 10), j = column, value = 4)      
    }
    toc(log = TRUE)
    
    cat(fill = TRUE, "frequency of each land cover class, after update")
    print(dt[, .N, by = y1987][order(y1987)]) # get freq again
    
    # return dt
    dt
    
  } else { 
    dt
  }
  
}




# -------------------------------------------------------------------------- #
# save processed data.tables showing age of abandonment as rasters
# -------------------------------------------------------------------------- #
 cc_save_dt_as_raster <- function(site, type, 
                                 input_path = p_dat_derived,
                                 output_path = p_dat_derived) {
  # load dt
  dt <- fread(file = paste0(input_path, site, type, ".csv"))
  
  # merge the data_table back to the original land cover data.table, if necessary
  if (grepl("age", type)){
    cat("Joining age data.table to input land cover data.table, filling non-abandonment pixels with NA", fill = TRUE)
    cat("Land cover data.table location:", paste0(input_path, site, ".csv"), fill = TRUE)
    lc_dt <- fread(file = paste0(input_path, site, ".csv"))
    dt <- merge(lc_dt[, .(x, y)], dt, all = TRUE, by = c("x", "y"), sort = FALSE)
  }
  
  # convert age dt to raster
  r <- dt_to_raster(dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
  
  # write raster
  writeRaster(r, filename = paste0(output_path, site, type, ".tif"), overwrite = TRUE)
  
  cat("Saved data.table as raster at:", paste0(output_path, site, type, ".tif"), fill = TRUE)
  
  
  # reload, and assign
  # brick(paste0(directory, name, "_age.tif"))
}

# I need to write some code to bind the original x and y columns from 
# the first raster to the age csv, so that even the non-abandonment rows 
# with NAs are included. 
# sp::points2grid()
# sp::coordinates()



# -------------------------------------------------------------------------- #
# calculate the maximum length of time abandoned (max age) for each pixel 
# -------------------------------------------------------------------------- #
cc_calc_max_age <- function(directory = p_dat_derived, site, run_label) {
  
  tic("load data")
  abn_age_dt <- fread(file = paste0(directory, site, "_age", run_label, ".csv"))
  toc(log = TRUE)
  
  tic("calculate max age in abn_age_dt")
  abn_age_dt[, max_age := max(.SD), .SDcols = -c("x", "y"), by = .(x, y)]
  toc(log = TRUE)
  
  cat(fill = TRUE, "Write out max age dt:", paste0(directory, site, "_max_age", run_label, ".csv"))
  tic("write out max age dt")
  fwrite(abn_age_dt[, .(x, y, max_age)], file = paste0(directory, site, "_max_age", run_label, ".csv"))
  toc(log = TRUE)
  
}



# meta functions ---- 

# -------------------------------------------------------------------------- #
# Save raw raster as data.table
# -------------------------------------------------------------------------- #
cc_r_to_dt <- function(site, 
                       input_path = p_dat_derived, 
                       output_path = p_dat_derived,
                       site_df) {
  # Convert raw rasters to data.tables, involving:
  # 1. loading raw raster
  # 2. updating names
  # 3. converting raw raster to raw data.table,
  # 4. writing raw dt to csv.
  # 5. recode raw data.table to correct land cover classes: cc_recode_lc_dt()
  # 6. write out the recoded dt to csv.
  
  # requires raster, data.table, devtools, tictoc, dtraster
  
  cat(fill = TRUE, "cc_r_to_dt(): Processing and recoding raw raster for site: ", site)
  cat(fill = TRUE, "Input path: ", input_path)
  cat(fill = TRUE, "Output path: ", output_path)
  
  # 1. load raw raster
  cat(fill = TRUE, "i. Start by processing raw input raster:", paste0(input_path, site, "_raw.tif"))
  tic("load raster")
  r <- brick(paste0(input_path, site, "_raw.tif"))
  toc()
  
  
  # 2. update the year column names, not trim... the raw data.tables should have all years of data.
  cat(fill = TRUE, "ii. Update raster layer names")
  cat(fill = TRUE, "raster layer names, before update:")
  print(names(r)) # before update
  
  tic("update raster layer names")
  if (site == "nebraska") {
    cat(fill = TRUE, "number of layers in ", site, ": ", nlayers(r))
    names(r) <- paste0("y", 1986:2018)
  } else {
    if (site == "wisconsin") {
      cat(fill = TRUE, "number of layers in ", site, ": ", nlayers(r))
      names(r) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      cat(fill = TRUE, "number of layers in ", site, ": ", nlayers(r))
      names(r) <- paste0("y", 1987:2017)
    }
  }
  
  cat(fill = TRUE, "raster layer names, after update:")
  print(names(r)) # after update
  toc(log = TRUE)
  
  
  # 3. load as a data.table - warning, this is very slow
  cat(fill = TRUE, "iii. Load as a data.table")
  tic("load as a data.table")
  dt <- as.data.table.raster(r)
  toc(log = TRUE)
  
  cat(fill = TRUE, "data.table column names:")
  print(names(dt))
  
  # 4. write out raw data.table as a csv
  cat(fill = TRUE, "iv. Write raw data.table to csv")
  tic("write out raw data.table as a csv")
  fwrite(dt, file = paste0(input_path, site, "_raw.csv"))
  toc(log = TRUE)
  
  
  # 5. recode land cover classes
  cat(fill = TRUE, "v. Recode land cover classes: cc_recode_lc_dt()")
  dt <- cc_recode_lc_dt(dt = dt, site = site, site_df = site_df)
  
  # 6. write out the recoded data.table, to be used as the final input for the abandonment age analysis
  cat(fill = TRUE, "vi. Write recoded data.table to csv")
  tic("write out recoded data.table as a csv")
  fwrite(dt, file = paste0(output_path, site, ".csv"))
  toc(log = TRUE)  
  
  
  cat(fill = TRUE, "Done! Converted raster to dt using cc_r_to_dt() for: ", site)
  cat(fill = TRUE, "New files can be found at:")
  cat(fill = TRUE, "raw raster, as a csv:", paste0(input_path, site, "_raw.csv"))
  cat(fill = TRUE, "recoded land cover, as a csv:",paste0(input_path, site, ".csv"))
  
}



# -------------------------------------------------------------------------- #
# Process data.tables to calculate and extract abandonment periods
# -------------------------------------------------------------------------- #
cc_filter_abn_dt_simple <- function(dt) {
  # 3. Update land cover classes to lump grassland and woody vegetation into 
  # single non-crop category, and binarize crop and noncrop
  cc_update_lc(dt, crop_code = 0, noncrop_code = 1)  # update land cover classes

  # 4. Remove NAs
  dt <- na.omit(dt)   # remove NAs
  
  # 5. Filter out the non-abandonment pixels (i.e. those that are either all crop or all noncrop)
  dt <- cc_remove_non_abn(dt)   # filter non abandonment pixels
  
  # 7. Pass temporal filter, to address potential cases of misclassification 
  # (five- and eight-year moving windows):
  cc_temporal_filter(dt, replacement_value = 1, filter_edge = TRUE)
  
  # 8. Calculate age of abandonment in each pixel that experiences abandonment
  cc_calc_age(dt)  # calculate age
  
  # 9. Erase non abandonment periods (i.e. those that start the time series as non-crop)
  cc_erase_non_abn_periods(dt)  # erase non abandonment periods
  
  dt[]
}

cc_filter_abn_dt <- function(site, select_1987_2017 = TRUE,
                             path = p_dat_derived,
                             pass_temporal_filter = TRUE, filter_edge = TRUE,
                             run_label = paste0("_", Sys.Date()), # format(Sys.time(), "_%Y-%m-%d_%H%M%S")
                             temporal_filter_replacement_value = 1) {
  
  # Steps:
  # 1. (re)load recoded dt
  # 2. update this code to select just 1987:2017, with a switch
  # 3. Update land cover classes to lump grassland and woody vegetation into a single non-crop category, and binarize crop and noncrop
  # 4. Remove NAs
  # 5. Filter out the non-abandonment pixels (i.e. those that are either all crop or all noncrop)
  # 6. Count recultivation blips, and clean (i.e. periods of recultivation that do not last longer than the recultivation threshold, which is by default 1 year)
  # 7. Pass temporal filter
  # 8. Calculate age of abandonment in each pixel that experiences abandonment
  # 9. Erase non abandonment periods (i.e. those that start the time series as non-crop)
  # 10. Write out cleaned abandonment age data.table
  # 11. Make diff data.table, each year subtracted by the previous year produces a data.table with year-to-year lagged differences (much like base::diff())
  # 12. Write out dt_diff
  # 13. Extract the length of each period of abandonment
  # 14. Create a data.table listing the lengths, write to file.
  
  # requires raster, data.table, devtools, tictoc, dtraster

  #tic("full script processing time")
  cat(fill = TRUE, "cc_filter_abn_dt(): Processing data.table for site: ", site)
  cat(fill = TRUE, "Path: ", path)
  cat(fill = TRUE, "run_label: ", run_label)
  if(pass_temporal_filter) cat(fill = TRUE, "Passing temporal filter, replacing with:", 
                               "temporal_filter_replacement_value =", temporal_filter_replacement_value)
  if(filter_edge) {
    cat(fill = TRUE, "Note: temporal filter filtered edge cases (beginning of time series only).")
  } else {
      cat(fill = TRUE, "Note: temporal filter did not filter edge cases.")}
    
  # -------------------------------------------------------- #
  # 1. load raw dt
  cat(fill = TRUE, "i. Start by loading raw data.table")
  tic("load the raw data.table")
  dt <- fread(input = paste0(path, site, ".csv"))
  toc(log = TRUE)
  
  cat(fill = TRUE, "data.table input:")
  print(dt) # print the head and tail of the data.table
  
  
  # 2. select only the 1987:2017, with a switch
  cat(fill = TRUE, "ii. Select years 1987:2017:", select_1987_2017)
  
  if(select_1987_2017) {
    if (site %in% c("nebraska", "wisconsin")) {
      cat(fill = TRUE, "dt names, before selection:")
      print(names(dt)) # before update
      dt <- dt[, c("x", "y", paste0("y", 1987:2017))]
      
      cat(fill = TRUE, "dt names, after selection:")
      print(names(dt)) # after update
    } else {
      cat(fill = TRUE, "dt names: no selection necessary")
      print(names(dt)) # after update
    }   
  } else {
    cat(fill = TRUE, "dt names:")
    print(names(dt)) # after update
    }
  
  
  # start heavy processing
  # 3. Update land cover classes to lump grassland and woody vegetation into 
  # single non-crop category, and binarize crop and noncrop
  cat(fill = TRUE, "iii. Update land cover classes, lumping grassland and woody_veg into 
      a single noncrop category (then binarizing noncrop and crop).")
  tic("updated land cover classes")
  cc_update_lc(dt, crop_code = 0, noncrop_code = 1)  # update land cover classes
  toc(log = TRUE)
  
  # 4. Remove NAs
  cat(fill = TRUE, "iv. Remove NAs.")
  tic("removed NAs")
  dt <- na.omit(dt)   # remove NAs
  toc(log = TRUE)
  
  # 5. Filter out the non-abandonment pixels (i.e. those that are either all crop or all noncrop)
  cat(fill = TRUE, "v. Filter non abandonment pixels.")
  tic("filter non abandonment pixels")
  dt <- cc_remove_non_abn(dt)   # filter non abandonment pixels
  toc(log = TRUE)
  
  # 6. Count the number of cases identified by the temporal filter:
  if(pass_temporal_filter) {
    tic("count cases identified by temporal filter")
    temporal_filter_counts_l <- cc_temporal_filter_count(dt = dt)

    # save files
    save(temporal_filter_counts_l, 
         file = paste0(output_path, site, "_temporal_filter_counts_l", run_label, ".rds"))
    toc(log = TRUE)
    
    cat(fill = TRUE, "vi. Used cc_temporal_filter_count() to count cases filtered by temporal filter, including edge cases.
        Wrote resulting list to: ", paste0(output_path, site, "_temporal_filter_counts_l", run_label, ".rds"))
  }
  
  # 7. Pass temporal filter, to address potential cases of misclassification 
  # (five- and eight-year moving windows):
  if(pass_temporal_filter) {
    tic("pass temporal filter")
    cc_temporal_filter(dt = dt, 
                       replacement_value = temporal_filter_replacement_value, 
                       filter_edge = filter_edge)
    
    cat(fill = TRUE, "vii. cc_temporal_filter(): passed temporal filter (5 and 8 year moving windows),
        with replacement value (", temporal_filter_replacement_value, ").", "Note:",
        if(filter_edge) {"also filtered edge cases (beginning only)."} else {"did not filter edge cases."})
    toc(log = TRUE)
    
  } else {
    warning("vi & vii. Did not pass temporal filters or count cases.")
  }
  
  # 8. Calculate age of abandonment in each pixel that experiences abandonment
  cat(fill = TRUE, "viii. Calculate age of abandonment for each pixel that experiences abandonment: cc_calc_age()")
  tic("calculated age")
  cc_calc_age(dt)  # calculate age
  toc(log = TRUE)
  
  # 9. Erase non abandonment periods (i.e. those that start the time series as non-crop)
  cat(fill = TRUE, "ix. Erase non abandonment periods (i.e. those that start the time series as non-crop): cc_erase_non_abn_periods()")
  tic("erased non abandonment periods")
  cc_erase_non_abn_periods(dt)  # erase non abandonment periods
  toc(log = TRUE)
  
  # 10. Write out cleaned abandonment age data.table
  cat(fill = TRUE, "x. Write out cleaned abandonment age data.table to:", paste0(path, site, "_age", run_label,".csv"))
  tic("wrote out cleaned abandonment age data.table")
  fwrite(dt, file = paste0(path, site, "_age", run_label,".csv"))
  toc(log = TRUE)
  
  # 11. Make diff data.table, each year subtracted by the previous year
  # produces a data.table with year-to-year lagged differences (much like base::diff())
  cat(fill = TRUE, "xi. Make dt_diff, with year-to-year lagged differences: cc_diff_dt()")
  tic("made diff")
  dt_diff <- cc_diff_dt(dt)
  toc(log = TRUE)
  
  # 12. Write out dt_diff
  cat(fill = TRUE, "xii. Write out dt_diff to:", paste0(path, site, "_diff", run_label,".csv"))
  tic("write out dt_diff")
  fwrite(dt_diff, file = paste0(path, site, "_diff", run_label,".csv"))
  toc(log = TRUE)
  
  # 13. Extract the length of each period of abandonment
  cat(fill = TRUE, "xiii. Extract the length of each period of abandonment: cc_extract_length()")
  tic("extracted length")
  length <- cc_extract_length(dt_diff)
  toc(log = TRUE)
  
  # 14. Create a data.table listing the lengths, write to file.
  cat(fill = TRUE, "xiv. Create a data.table listing the lengths, write to:", paste0(path, site, "_length", run_label,".csv"))
  tic("created data.table of lengths")
  length <- data.table(length = length)
  toc(log = TRUE)
  
  tic("wrote out length data.table")
  fwrite(length, file = paste0(path, site, "_length", run_label,".csv"))
  toc(log = TRUE)
  

  cat(fill = TRUE, "Done! cc_filter_abn_dt() for: ", site)
}






# area/persistence functions ---- 

# -------------------------------------------------------------------------- #
# calculate the total area in:
# each land cover class in the original land cover data, and
# that is abandoned, over time
# -------------------------------------------------------------------------- #
cc_calc_area_per_lc_abn <- function(land_cover_dt, abn_age_dt, land_cover_raster, 
                                    abandonment_threshold = 5) {
  # names(abn_age_dt) <- c("x","y", paste0("y", 1987:2017))
  col_names <- grep("x$|y$", names(land_cover_dt), value = TRUE, invert = TRUE)
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  area_dt <- as.data.table.raster(area_raster) # convert to data.table
  setnames(area_dt, old = "layer", new = "pixel_area") # update layer names
  
  # round x and y columns to deal with floating point imprecision:
  round_digits1 <- 11

  while(merge(x = abn_age_dt[, 1:3], y = area_dt, 
              all.x = TRUE, by = c("x", "y"), sort = FALSE
  )[is.na(pixel_area), .N] > 0) {
    round_digits1 <- round_digits1 - 1
    abn_age_dt[, ':='(x = round(x, round_digits1), y = round(y, round_digits1))]
    area_dt[, ':='(x = round(x, round_digits1), y = round(y, round_digits1))]
  }
  cat(fill = TRUE, "Note: abn_age_dt x and y columns rounded to", round_digits1, "digits to facilitate merge.")
  
  round_digits2 <- 11
  while(merge(x = land_cover_dt[, 1:3], y = area_dt, 
              all.x = TRUE, by = c("x", "y"), sort = FALSE
  )[is.na(pixel_area), .N] > 0) {
    round_digits2 <- round_digits2 - 1
    land_cover_dt[, ':='(x = round(x, round_digits2), y = round(y, round_digits2))]
    area_dt[, ':='(x = round(x, round_digits2), y = round(y, round_digits2))]
  }
  cat(fill = TRUE, "Note: land_cover_dt x and y columns rounded to", round_digits2, "digits to facilitate merge.")
  
  # merge area to abn_age_dt and land_cover_dt, based on the x and y coordinates, 
  # saving only those rows in area_dt that match abn_age_dt and land_cover_dt, respectively:
  abn_age_dt <- merge(x = abn_age_dt, y = area_dt, all.x = TRUE, 
                      by = c("x","y"), sort = FALSE)
  land_cover_dt <- merge(x = land_cover_dt, y = area_dt, all.x = TRUE, 
                         by = c("x","y"), sort = FALSE)
  
  
  if(abn_age_dt[is.na(pixel_area), .N] + 
      land_cover_dt[is.na(pixel_area), .N] > 0 ) {stop("Merge did not match at 100%")}
  
  for (i in seq_along(col_names)) {
    temp_dt <- 
      land_cover_dt[, by = c(col_names[i]), # by land cover categories in column i,
                    
                    # sum all pixel_area values (km2) and multiply by 100 to get total area in ha
                    .("total_area" = sum(pixel_area) * 100)  
                    
                    # then reorder by lc class
                    ][order(get(col_names[i]))] 
    
    setnames(temp_dt, old = col_names[i], new = "lc")
    setnames(temp_dt, old = "total_area", new = col_names[i]) # rename the total_area column to that specific year
    
    if(i > 1) {
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
    pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "area_ha") %>%
    mutate(year = as.integer(gsub("y", "", year))) %>%
    select(year, lc, area_ha)
  
  abandoned_area_df_threshold <- tibble(
    year = 1987:2017,
    lc = paste0("Abandoned (>=", abandonment_threshold, ")"),
    area_ha = sapply(1:31, function(i) {
      abn_age_dt[get(paste0("y", 1987:2017)[i]) >= abandonment_threshold, 
                 sum(pixel_area) * 100]
      }
      )
    )
  
  
  abandoned_area_df_all <- tibble(
    year = 1987:2017,
    lc = "Abandoned (>1)",
    area_ha = sapply(1:31, function(i) {
      abn_age_dt[get(paste0("y", 1987:2017)[i]) > 0, 
                 sum(pixel_area) * 100]
      }
      )
    )
  
  # return the tibble
  area_df <- bind_rows(lc_area_df, abandoned_area_df_threshold, abandoned_area_df_all)
  
}

# ------------------------------------------------------------------------------------ #
# calculate area of abandoned land over time, for a particular abn_age_dt
# ------------------------------------------------------------------------------------ #

# idea: 2.12.2021: have a column with the area of each pixel. subset a year column by a class, 
# and sum the second column of the areas to calculate the total area.

cc_calc_abn_area <- function(abn_age_dt, land_cover_raster, abandonment_definition = 5) {
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  area_dt <- as.data.table.raster(area_raster) # convert to data.table
  setnames(area_dt, old = "layer", new = "pixel_area") # update layer names
  
  # round x and y columns to deal with floating point imprecision:
  round_digits <- 11
  
  while(merge(x = abn_age_dt[, 1:3], y = area_dt, 
              all.x = TRUE, by = c("x", "y"), sort = FALSE
  )[is.na(pixel_area), .N] > 0) {
    round_digits <- round_digits - 1
    abn_age_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))]
    area_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))]
  }
  
  # merge area to the abn_age_dt, based on the x and y coordinates, 
  abn_age_dt <- merge(x = abn_age_dt, y = area_dt, all.x = TRUE, by = c("x", "y"), sort = FALSE)
  
  if(abn_age_dt[is.na(pixel_area), .N] > 0) {stop("Merge did not match at 100%")}
  
  cat(fill = TRUE, "Note: x and y columns rounded to", round_digits, "digits to facilitate merge.")
  abandoned_area_df <- tibble(
    year = 1987:2017,
    lc = "Abandoned",
    count = sapply(paste0("y", 1987:2017), function(i) {abn_age_dt[get(i) >= abandonment_threshold, .N]}),
    count_all = sapply(paste0("y", 1987:2017), function(i) {abn_age_dt[get(i) > 0, .N]}),
    
    area_ha = 100 * sapply(paste0("y", 1987:2017), function(i) {abn_age_dt[get(i) >= abandonment_threshold, sum(pixel_area)]}),
    area_all_ha = 100 * sapply(paste0("y", 1987:2017), function(i) {abn_age_dt[get(i) > 0, sum(pixel_area)]})
  )
}



# ------------------------------------------------------------------------------------ #
# calculate persistence of abandoned land over time, either as a raw count or as a percentage
# ------------------------------------------------------------------------------------ #
cc_calc_persistence <- function(abn_age_dt, 
                                land_cover_raster,
                                abandonment_threshold = 5) {
  if(length(abn_age_dt) > 33) {stop("Code is designed to work with abn_age_dt for just 1987:2017.")}
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  area_dt <- as.data.table.raster(area_raster) # convert to data.table
  setnames(area_dt, old = "layer", new = "pixel_area") # update layer names
  
  # round x and y columns to deal with floating point imprecision between area_dt and abn_age_dt:
  round_digits <- 11
  
  while(merge(x = abn_age_dt[, 1:3], y = area_dt,  # conditional test
              all.x = TRUE, by = c("x", "y"), sort = FALSE
  )[is.na(pixel_area), .N] > 0) {
    round_digits <- round_digits - 1
    abn_age_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))] # rounding step
    area_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))] # rounding step
  }
  
  # merge area to the abn_age_dt, based on the x and y coordinates, 
  abn_age_dt <- merge(x = abn_age_dt, y = area_dt, all.x = TRUE, by = c("x", "y"), sort = FALSE)
  
  if(abn_age_dt[is.na(pixel_area), .N] > 0) {stop("Merge did not match at 100%")}
  
  cat(fill = TRUE, "Note: x and y columns rounded to", round_digits, "digits to facilitate merge.")
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  persistence_list <- lapply(1:30, function(j) {
    # calculate a vector of the summed area (ha) of abandoned pixels that originate 
    # in a particular year (i.e. cohort), and the area (ha) of those pixels in each 
    # subsequent year, starting in 1988. 
    # Filled with NAs for periods that are beyond the length of the time series.
    
    temp_vector <- c(rep(NA, j),
                     sapply(1:(31 - j), function(i) {
                       abn_age_dt[get(paste0("y", 1987:2017)[i + j]) == i, 
                                  sum(pixel_area) * 100] # calculate area in km2, then convert to ha
                     }))
    
    # remove cells that are below abandonment threshold:
    if (abandonment_threshold > 1) {
      temp_vector[c(1:(abandonment_threshold - 1) + j)] <- NA
      if(length(temp_vector) > 31){temp_vector <- temp_vector[c(1:31)]}
    }
    
    # return vector
    temp_vector
  }
  )
  
  # combine list into a data.frame
  persistence_df <- data.frame(
    do.call("cbind", persistence_list))
  
  names(persistence_df) <- paste0("y", 1988:2017)
  
  persistence_df <- persistence_df %>%
    mutate(year = 1987:2017) %>%
    select(year, everything()) %>%
    
    # pivot to long format
    pivot_longer(cols = paste0("y", 1988:2017), 
                 names_to = "year_abn", 
                 values_to = "area_ha",
                 values_drop_na = TRUE) %>%
    mutate(year_abn = as.integer(gsub("y", "", year_abn))) %>% 
    
    # calculate proportion remaining
    group_by(year_abn) %>% mutate(proportion = area_ha / max(area_ha)) %>% ungroup() %>% 
    
    # calculate age bins, keeping in mind that these bins are [) 
    # i.e. closed on the left side, open on the right 5 <= x < 10
    mutate(age = year - year_abn + 1,
           bins = cut(age, breaks = c(0, 5, 10, 15, 20, 25, 30), right = FALSE, include.lowest = TRUE),
           bins = as_factor(bins))
  
  persistence_df
}


# ------------------------------------------------------------------------------------ #
# save all permutations of abandonment persistence
# old, no longer used: only good for previous iteration of cc_calc_persistence prior to 
# 3.4.2021
# ------------------------------------------------------------------------------------ #
# 
# cc_calc_persistence_all <- function(abn_age_dt, 
#                                     land_cover_raster, 
#                                     include_wide = FALSE,
#                                     abandonment_threshold = 5,
#                                     include_all_abandonment = TRUE){
# 
#   count <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                land_cover_raster = land_cover_raster, 
#                                stat_proportion = FALSE, NA_first = FALSE, 
#                                include_wide = include_wide,
#                                abandonment_threshold = abandonment_threshold)
#   
#   proportion <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                     land_cover_raster = land_cover_raster, 
#                                     stat_proportion = TRUE, NA_first = FALSE, 
#                                     include_wide = include_wide,
#                                     abandonment_threshold = abandonment_threshold)
#   
#   count_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                         land_cover_raster = land_cover_raster, 
#                                         stat_proportion = FALSE, NA_first = TRUE, 
#                                         include_wide = include_wide,
#                                         abandonment_threshold = abandonment_threshold)
#   
#   proportion_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                              land_cover_raster = land_cover_raster, 
#                                              stat_proportion = TRUE, NA_first = TRUE, 
#                                              include_wide = include_wide,
#                                              abandonment_threshold = abandonment_threshold)
#   # na_first <- left_join(x = count_na_first, y = proportion_na_first, 
#   #                       by = c("year", "year_abn", "age", "bins"))
#   
#   if(include_all_abandonment) {
#     count_all <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                  land_cover_raster = land_cover_raster, 
#                                  stat_proportion = FALSE, NA_first = FALSE, 
#                                  include_wide = include_wide,
#                                  abandonment_threshold = 1)
#     
#     proportion_all <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                       land_cover_raster = land_cover_raster, 
#                                       stat_proportion = TRUE, NA_first = FALSE, 
#                                       include_wide = include_wide,
#                                       abandonment_threshold = 1)
#     
#     count_all_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                           land_cover_raster = land_cover_raster, 
#                                           stat_proportion = FALSE, NA_first = TRUE, 
#                                           include_wide = include_wide,
#                                           abandonment_threshold = 1)
#     
#     proportion_all_na_first <- cc_calc_persistence(abn_age_dt = abn_age_dt, 
#                                                land_cover_raster = land_cover_raster, 
#                                                stat_proportion = TRUE, NA_first = TRUE, 
#                                                include_wide = include_wide,
#                                                abandonment_threshold = 1)
#     
#   }
# 
#   # return the products as a list
#   if(include_wide) {
#     c(
#       list(
#         count = count,
#         count_na_first = count_na_first,
#         proportion = proportion,
#         proportion_na_first = proportion_na_first
#          ),
#       if(include_all_abandonment) {
#         list(
#           count_all = count_all,
#           count_all_na_first = count_all_na_first,
#           proportion_all = proportion_all,
#           proportion_all_na_first = proportion_all_na_first
#         )
#       }
#       )
#     } else {
#       # join the dfs
#       
#       na_last <- left_join(x = count, y = proportion, 
#                          by = c("time_abn", "year_abn"))
#       na_first <- left_join(x = count_na_first, y = proportion_na_first, 
#                           by = c("year", "year_abn", "age", "bins"))
#       
#       if(include_all_abandonment) {
#         na_last_all <- left_join(x = count_all, y = proportion_all, 
#                              by = c("time_abn", "year_abn"))
#         na_first_all <- left_join(x = count_all_na_first, y = proportion_all_na_first, 
#                               by = c("year", "year_abn", "age", "bins"))
#       }
#     
#       # return list
#       c(
#         list(na_last = na_last,
#              na_first = na_first),
#         if(include_all_abandonment) {
#           list(
#             na_last_all = na_last_all,
#             na_first_all = na_first_all
#           )
#           }
#       )
#     }
# }


# ------------------------------------------------------------------------------------ #
# calculate gains and losses of abandoned land over time (turnover)
# ------------------------------------------------------------------------------------ #
cc_calc_abn_diff <- function(abn_age_dt, land_cover_raster,
                                  abandonment_threshold = 5) {
  
  area_raster <- raster::area(land_cover_raster) # calculate area in km2
  area_dt <- as.data.table.raster(area_raster) # convert to data.table
  setnames(area_dt, old = "layer", new = "pixel_area") # update layer names
  
  # round x and y columns to deal with floating point imprecision between area_dt and abn_age_dt:
  round_digits <- 11
  
  while(merge(x = abn_age_dt[, 1:3], y = area_dt,  # conditional test
              all.x = TRUE, by = c("x", "y"), sort = FALSE
  )[is.na(pixel_area), .N] > 0) {
    round_digits <- round_digits - 1
    abn_age_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))] # rounding step
    area_dt[, ':='(x = round(x, round_digits), y = round(y, round_digits))] # rounding step
  }
  
  # merge area to the abn_age_dt, based on the x and y coordinates, 
  abn_age_dt <- merge(x = abn_age_dt, y = area_dt, all.x = TRUE, by = c("x", "y"), sort = FALSE)
  
  if(abn_age_dt[is.na(pixel_area), .N] > 0) {stop("Merge did not match at 100%")}
  
  cat(fill = TRUE, "Note: x and y columns rounded to", round_digits, "digits to facilitate merge.")
  
  
  # first calculate a list of 30 vectors corresponding to abandonment originating in a particular year.
  abn_turnover_list <- lapply(1:30, function(j) {
    # Calculate a vector of counts of abandoned pixels that originate in a particular year, 
    # and the count of those pixels in each year following, starting in 1988.
    # Filled with 0s for periods that are beyond the length of the time series.
    # Then, calculate the difference year-to-year for each abandonment cohort.
    
    # count the number of rows where age = 1 in 1st year of abandonment, 
    # age = 2 in 2nd, age = 3 in 3rd, etc. For js that increase progressively, 
    # representing different cohorts of abandonment (i.e. year of first abandonment). 
    # E.g. j = 1 represents 1988, and the vector represents the amount of land from that 
    # cohort that is abandoned in 1988, 1989, 1990, etc. 
    
    temp_vector_diff <- c(
      rep(0, j),
      sapply(1:(31 - j), function(i) {
        abn_age_dt[get(paste0("y", 1987:2017)[i + j]) == i, sum(pixel_area) * 100] # calculate the area in ha
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
                 values_to = "area_ha",
                 values_drop_na = TRUE) %>%
    mutate(year_abn = as.integer(gsub("y", "", year_abn)))
  
  
  # calculate net, gain, and loss:
  # net gain in abandoned land area
  abn_area_net <- abn_turnover_long %>%
    group_by(year) %>% 
    summarise(net = sum(area_ha))
  
  # gain
  abn_area_gain <- abn_turnover_long %>%
    group_by(year) %>% 
    filter(area_ha > 0) %>%
    summarise(gain = sum(area_ha))
  
  # loss
  abn_area_loss <- abn_turnover_long %>%
    group_by(year) %>% 
    filter(area_ha < 0) %>%
    summarise(loss = sum(area_ha))
  
  turnover_df <- abn_area_net %>% 
    full_join(., abn_area_gain, by = "year") %>%
    full_join(., abn_area_loss, by = "year") %>%
    pivot_longer(cols = c("net", "gain", "loss"),
                 names_to = "change", values_to = "area_ha",
                 values_drop_na = TRUE)
  
  turnover_df
  
}



# -------------------------------------------------------------------------- #
# Summarize abandonment data.tables, generating data.frames to use for plotting
# -------------------------------------------------------------------------- #

# formerly: cc_generate_dfs()
cc_summarize_abn_dts <- function(input_path,
                                 output_path,
                                 site,
                                 run_label = paste0("_", Sys.Date()),
                                 abandonment_threshold = 5,
                                 include_all = FALSE) {
  cat(fill = TRUE, "cc_summarize_abn_dts(): Summarizing results for site: ", site)
  cat(fill = TRUE, "input_path: ", input_path)
  cat(fill = TRUE, "output_path: ", output_path)
  cat(fill = TRUE, "run_label: ", run_label)
  cat(fill = TRUE, "abandonment_threshold: >=" , abandonment_threshold)

  # load files:
  lc_r <- raster(paste0(input_path, site, ".tif"))
  lc_dt <- fread(input = paste0(input_path, site, ".csv")) 
  age_dt <- fread(input = paste0(input_path, site, "_age", run_label, ".csv"))
  
  cat("calculating total area in each land cover class, and that is abandoned (for at least as long as the abandonment threshold), over time.", fill = TRUE)
  # ------------- calculate total area per lc, with abandonment ---------------- #
  area <- cc_calc_area_per_lc_abn(land_cover_dt = lc_dt, 
                                  abn_age_dt = age_dt, 
                                  land_cover_raster = lc_r,
                                  abandonment_threshold = abandonment_threshold)
  
  cat("calculate abandonment persistence", fill = TRUE)
  # ------------------------ abandonment persistence --------------------------- #
  persistence <- cc_calc_persistence(abn_age_dt = age_dt, 
                                          land_cover_raster = lc_r,
                                          abandonment_threshold = abandonment_threshold)
  
  cat("calculate abandonment area turnover", fill = TRUE)
  # -------------------- calculate the abandonment area turnover ------------------- #
  turnover <- cc_calc_abn_diff(abn_age_dt = age_dt, 
                                           land_cover_raster = lc_r,
                                           abandonment_threshold = abandonment_threshold)
  
  # save individual results_df
  write_csv(area, file = paste0(path, site, "_result_area", run_label,".csv"))
  write_csv(persistence, file = paste0(path, site, "_result_persistence", run_label,".csv"))
  write_csv(turnover, file = paste0(path, site, "_result_turnover", run_label,".csv"))
  
  # updating object names names
  assign(paste0("area", run_label), area)
  assign(paste0("persistence", run_label), persistence)
  assign(paste0("turnover", run_label), turnover)
  
  cat("saving files:", paste0(output_path, site, "_result_dfs", run_label, ".rds"), fill = TRUE)
  # save files
  save(list = c(paste0("area", run_label), 
                paste0("persistence", run_label),
                paste0("turnover", run_label)
                ), 
       file = paste0(output_path, site, "_result_dfs", run_label, ".rds")
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
                                     width = 5, height = 4, save_all = FALSE,
                                     output_path) {
  
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
  png(filename = paste0(output_path, "area_lc_abn", 
                        outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_lc_abn_area)
  dev.off()

}

# ------------------------------------------------------------------------------------ #
# plot persistence of abandonment over time
# ------------------------------------------------------------------------------------ #

cc_save_plot_abn_persistence <- function(persistence_df, subtitle, outfile_label,
                                         width = 7, height = 5,
                                         save_all = TRUE, subtitle_all = NULL,
                                         output_path) {
  
  gg_base <- ggplot(data = persistence_df) + 
    theme_classic() + 
    labs(title = "Persistence of Abandoned Land",
         subtitle = subtitle,
         color = "Year Abandoned") + 
    scale_color_distiller(palette = "Greens") + theme(legend.position = "bottom") +
    theme(legend.text = element_text(angle = 320, vjust = 1, hjust = 0))
  
  # raw area
  gg_persistence_count <- gg_base + 
    geom_line(mapping = aes(x = age, y = area_ha / 10^3,
                            group = year_abn, color = year_abn), 
              size = 1.25) + 
    labs(y = expression("Area abandoned (10"^{3}*" ha)") , 
         x = "Years since initial abandonment")
    
  # as percentage
  gg_persistence_proportion <- gg_base + 
    geom_line(mapping = aes(x = age, y = proportion,
                            group = year_abn, color = year_abn), 
              size = 1.25) + 
    labs(y = "Proportion remaining abandoned", 
         x = "Years since initial abandonment")
  
  # na_first ----------- #
  gg_persistence_count_na_first <- gg_base + 
    geom_line(mapping = aes(x = year, y = area_ha / 10^3,
                            group = year_abn, color = year_abn), 
              size = 1.25) + 
    labs(y = expression("Area abandoned (10"^{3}*" ha)") , 
         x = "Year")
  
  gg_persistence_proportion_na_first <- gg_base + 
    geom_line(mapping = aes(x = year, y = proportion,
                            group = year_abn, color = year_abn), 
              size = 1.25) + 
    labs(y = "Proportion remaining abandoned", 
         x = "Year")
  
  
  # save
  png(filename = paste0(output_path, "persistence_", 
                        "count", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_count)
  dev.off()
  
  png(filename = paste0(output_path, "persistence_", 
                        "proportion", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_proportion)
  dev.off()
  
  png(filename = paste0(output_path, "persistence_", 
                        "count_na_first", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_count_na_first)
  dev.off()
  
  png(filename = paste0(output_path, "persistence_", 
                        "proportion_na_first", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  
  print(gg_persistence_proportion_na_first)
  dev.off()
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(output_path, "persistence_", 
                          "count_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_count %+% input_list$na_first_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(output_path, "persistence_", 
                          "proportion_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_proportion %+% input_list$na_first_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(output_path, "persistence_", 
                          "count_na_first_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    
    print(gg_persistence_count_na_first %+% input_list$na_first_all + 
            labs(subtitle = subtitle_all))
    dev.off()
    
    png(filename = paste0(output_path, "persistence_", 
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
                                        save_all = TRUE, subtitle_all = paste0(subtitle, ", all abandonment"),
                                        output_path) {
  
  # gain, loss, and net change in abandoned area, over time
  gg_turnover_base <- ggplot() + 
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
  
  gg_turnover <- gg_turnover_base +
    geom_col(data = filter(input_area_change_df, change != "net"),
             mapping = aes(x = year, y = area_ha / (10^3), 
                           group = change, fill = change)) + 
    geom_line(data = filter(input_area_change_df, change == "net"),
              mapping = aes(x = year, y = area_ha / (10^3), color = "Net Change in Area"),
              size = 1.5)
  
  if(save_all){
    gg_turnover_all <- gg_turnover_base +
      geom_col(data = filter(input_area_change_df, change != "net"),
               mapping = aes(x = year, y = area_ha_all / (10^3), 
                             group = change, fill = change)) + 
      geom_line(data = filter(input_area_change_df, change == "net"),
                mapping = aes(x = year, y = area_ha_all / (10^3), color = "Net Change in Area"),
                size = 1.5)
  }
  
  # save to file
  png(filename = paste0(output_path, "turnover", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(gg_turnover)
  dev.off()
  
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(output_path, "turnover_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(gg_turnover_all)
    dev.off()
  }
  
}


# ------------------------------------------------------------------------------------ #
# plot area of abandonment, by age class
# ------------------------------------------------------------------------------------ #
cc_save_plot_area_by_age_class <- function(persistence_df, subtitle, outfile_label,
                                           width = 7, height = 5,
                                           save_all = TRUE,
                                           output_path) {
  
  age_class_base <- ggplot(data = persistence_df) + 
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
  png(filename = paste0(output_path, "abn_area_by_class_cont", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(age_class_continuous)
  dev.off()
  
  png(filename = paste0(output_path, "abn_area_by_class_bins", outfile_label, ".png"), 
      width = width, height = height, units = "in", res = 400)
  print(age_class_bins)
  dev.off()
  
  
  # save plots with all abandonment cells, regardless of abandonment threshold
  if(save_all) {
    png(filename = paste0(output_path, "abn_area_by_class_cont_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(age_class_continuous %+% input_list$na_first_all)
    dev.off()
    
    png(filename = paste0(output_path, "abn_area_by_class_bins_all", outfile_label, ".png"), 
        width = width, height = height, units = "in", res = 400)
    print(age_class_bins %+% input_list$na_first_all)
    dev.off()
  }
}


# ------------------------------------------------------------------------------------ #
# save four plot types, master function
# ------------------------------------------------------------------------------------ #

# save just the general intro plots
cc_save_area_persistence_plots <- function(input_path, 
                                           output_path,
                                           site_label,
                                           run_label,
                                           outfile_label,
                                           subtitle, 
                                           subtitle_all = paste0(subtitle, ", all abandonment"), 
                                           save_all = TRUE) {
  
  # load the data
  load(file = paste0(input_path, site, "_result_dfs", run_label, ".rds"), verbose = TRUE)

  # ------------- calculate total area per lc, with abandonment ---------------- #
  cc_save_plot_lc_abn_area(input_area_df = eval(parse(text = paste0("area", run_label))), 
                           subtitle = subtitle, outfile_label = outfile_label,
                           save_all = save_all,
                           output_path = output_path,
                           width = 6, height = 5)
  
  
  # ------------------------ abandonment persistence --------------------------- #
  cc_save_plot_abn_persistence(input_list = eval(parse(text = paste0("persistence", run_label))), 
                               subtitle = subtitle, outfile_label = outfile_label,
                               save_all = save_all, subtitle_all = subtitle_all,
                               output_path = output_path,
                               width = 5, height = 5)
  
  
  # -------------------- calculate the abandonment area turnover ------------------- #
  cc_save_plot_area_gain_loss(input_area_change_df = eval(parse(text = paste0("turnover", run_label))), 
                              subtitle = subtitle, outfile_label = outfile_label,
                              save_all = save_all,
                              subtitle_all = subtitle_all,
                              output_path = output_path,
                              width = 5, height = 5)
  
  
  # -------------------- plot abandonment area by age class ------------------- #
  cc_save_plot_area_by_age_class(input_list = eval(parse(text = paste0("persistence", run_label))), 
                                 subtitle = subtitle, outfile_label = outfile_label,
                                 save_all = save_all,
                                 output_path = output_path,
                                 width = 6, height = 5)
  
  cat("Plots saved to:", output_path)
}



# fragmentation functions ----

# ------------------------------------------------------------------------------------ #
# merge and clean the fragmentation results
# ------------------------------------------------------------------------------------ #
cc_clean_frag_results <- function(site, n_metrics = 11, drop_0 = FALSE) {
  for (run in 1:n_metrics) {
    load(file = paste0(p_output, "frag/frag_", site, run, ".rds"),
         verbose = TRUE)
  }
  
  # make sure metrics_list is up-to-date
  metrics_list <- c(
    "lsm_c_ai", # aggregation index, class level (RS has used this one)
    "lsm_c_clumpy", # clumpiness index, class (maybe)
    
    "lsm_c_np", # number of patches, class
    "lsm_c_area_cv", # patch area, cv, per class
    "lsm_c_area_mn", # patch area, mean, per class
    "lsm_c_area_sd", # patch area, sd, per class
    "lsm_c_ca", # total (class) area
    
    "lsm_c_te", # total edge
    "lsm_c_para_cv", # perimeter-area ratio, cv
    "lsm_c_para_mn", # perimeter-area ratio, mean
    "lsm_c_para_sd", # perimeter-area ratio, sd
    
    # new additions, # 12-15
    "lsm_c_cohesion", # COHESION is an 'Aggregation metric'. It characterises the connectedness of patches belonging to class i. 
    # It can be used to asses if patches of the same class are located aggregated or rather isolated and thereby 
    # COHESION gives information about the configuration of the landscape. 
    
    "lsm_c_contig_mn", # Shape metric - Measures the "contiguity" of cells within patches (the class level metric is the mean across all patches in a class)
    "lsm_c_contig_cv",
    "lsm_c_contig_sd"
  )
  
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


# ------------------------------------------------------------------------------------ #
# save fragmentation plots
# ------------------------------------------------------------------------------------ #

cc_save_frag_plots <- function(input = frag_dat, 
                               outfile_label) {
  
  # load(file = paste0(p_output, "frag/frag_dat.rds"), verbose = TRUE)
  
  
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
    # geom_point(mapping = aes(x = year, y = value/(10^6), color = land_cover)) + 
    geom_line(mapping = aes(x = year, y = value/(10^6), color = land_cover), size = 1.2) + 
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

    
  # ------------------- 12. Cohesion Index -------------------- #
  # this measures the connectedness of patches in each class.
  gg_frag_cohesion <- gg_frag_base %+% filter(input, metric == "cohesion") +
    labs(title = "Patch Cohesion Index, by land cover", y = "Cohesion Index Value")
  
  
  
  # ------------------- 13-15. Contiguity -------------------- #
  gg_frag_contig_combo <- gg_frag_base %+%
    filter(input, metric %in% c("contig_cv", "contig_mn", "contig_sd")) +
    labs(title = "Contiguity Index, by land cover", y = "Contiguity Index") + 
    facet_grid(cols = vars(site), rows = vars(metric), scales = "free_y",
               labeller = labeller(metric = c(contig_cv = "Coeffient of Variation (cv = sd/mean)", 
                                              contig_mn = "Mean",  
                                              contig_sd = "Standard Deviation (sd)")))

  
  
  
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
  print(plot_grid(gg_frag_patch_area_mn, gg_frag_np, gg_frag_ca, nrow = 3))
  dev.off()

  png(filename = paste0(p_output, "plots/frag_class_area", outfile_label, ".png"), 
      width = 7, height = 4, units = "in", res = 400)
  print(gg_frag_ca)
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
  
  # cohesion index
  png(filename = paste0(p_output, "plots/frag_cohesion", outfile_label, ".png"), 
      width = 7, height = 4, units = "in", res = 400)
  print(gg_frag_cohesion)
  dev.off()
  
  # contiguity index
  png(filename = paste0(p_output, "plots/frag_contig_combo", outfile_label, ".png"), 
      width = 7, height = 8, units = "in", res = 400)
  print(gg_frag_contig_combo)
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
  plot(s_max_age_r, main = "Shaanxi: Maximum \nTime abandoned (years)",
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
  plot(b_max_age_r, main = "Belarus: Maximum \nTime abandoned (years)",
       maxpixels = maxpixels)
  
  dev.off()
}





