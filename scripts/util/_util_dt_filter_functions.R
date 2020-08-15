# ---------------------------------------------------------------
#
# abandonment data.table filtering functions
# 
# ---------------------------------------------------------------

# ---------------- Update land cover classes values -------------------------

cc_update_lc <- function(dt) {
  
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
      set(dt, which(dt[[x]] == 0), x, NA)     # set 0 values to NA, to be later removed with na.omit()
    } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 1), x, NA)     # set nonveg (urban, water, etc.) to NA
      } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 3), x, 1)      # set crop from 3 to 1
      } 
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 4), x, 2)      # combine 4 (grassland) and 2 (woody)
                                              # into a single noncrop layer (2)
      }
    } else { 
      # If the data.table doesn't have x y coordinates, use the following:
      # might be slightly faster if x and y are removed first, allowing the following
      
      for (x in seq_len(length(dt))) {
        set(dt, which(dt[[x]] == 0), x, NA)   # set 0 values to NA
        }     
      
      for (x in seq_len(length(dt))) {
        set(dt, which(dt[[x]] == 1), x, NA)   # set nonveg (urban, water, etc.) to NA
        }
      
      for (x in seq_len(length(dt))) {
        set(dt, which(dt[[x]] == 3), x, 1)    # set crop from 3 to 1
        }
      
      for (x in seq_len(length(dt))) {
        set(dt, which(dt[[x]] == 4), x, 2)    # combine 4 (grassland) and 2 (woody)
                                              # into a single noncrop layer (2)
      }
    }
}





# ---------------- Make data.table binary ------------------
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





# ---------------- Filter out pixels that are either all crop or all noncrop -------------------------
cc_remove_non_abn <- function(dt) {
  # must be used in the format:
  # dt <- cc_remove_non_abn(dt) 
  
  dt[dt[, rowSums(.SD) > 0 & rowSums(.SD) < length(.SD)], ] 
  # this is inefficient with memory, but since I'll be in adroit anyways, it'll probably be fine.
  # all crop row sums = 0
  # all noncrop row sums = length(dt) (i.e. the number of columns)
}




# ---------------- Calculate age of each noncrop cell -------------------------

cc_calc_age <- function(dt) {
  for (i in 2:ncol(dt)) {
    
    # subset rows that are greater than 0 (i.e. 1, for noncrop), and
    dt[get(names(dt)[i]) > 0, 
       
       # set them equal to the previous column's value in that row, plus 1.
       names(dt)[i] := get(names(dt)[i-1]) + 1] 
  }
}




# ---------------- Erase noncrop non-abn periods -----------------------------

# set any value that is equal to the column number to 0. 
# This removes age values for noncrop vegetation that start time-series as noncrop - 
# this can't be classified as abandonment, since we don't know what came before the time-series.

cc_erase_nonabn_periods <- function(dt) {
  
  # iterate across columns
  for (i in seq_len(length(dt))) {
    dt[get(names(dt)[i]) == i,  # filter rows with values equal to column number
       names(dt)[i] := 0] # set value to 0
  }
  
}


# ---------------- Make diff -------------------------

cc_diff_dt <- function(dt){
  # produces a data.table with year-to-year lagged differences (much like base::diff())
  dt_lead <- copy(dt)
  dt_lead[, names(dt_lead)[1] := NULL][, end := 0]
  dt_diff <- dt_lead - dt
}



# ---------------- Extract lengths of all abandonment periods -----------------------------

cc_extract_lengths <- function(dt_diff) {
  # note: this function only works with diff'd data.table, so that 
  # negative values mark years of recultivation (or the end of the time-series)
  abn_lengths <- vector(mode = "numeric")
  for(i in seq_len(length(dt_diff))) {
    abn_lengths <- c(abn_lengths, 
                     dt_diff[get(names(dt_diff)[i]) < 0,
                             get(names(dt_diff)[i])])
  }
  abs(abn_lengths)
}


# remove NAs from data.table
# f_dowle2 <- function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), (i):=0]
# }


