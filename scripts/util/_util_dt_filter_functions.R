# --------------------------------------------------------------- #
#
# abandonment data.table filtering functions
# 
# --------------------------------------------------------------- #



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
  names(r) <- gsub(gsub_pattern, "y", names(r))
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
