# ---------------------------------------------------------------
#
# abandonment data.table filtering functions
# 
# ---------------------------------------------------------------

# ---------------- Update land cover classes values -------------------------

cc_update_lc <- function(dt, make_binary = FALSE) {
  
  # Original Land use class codes:
  #       1. Non-vegetated area (e.g. water, urban, barren land)
  #       2. Woody vegetation
  #       3. Cropland 
  #       4. Herbaceous land (e.g. grassland)
  
  # update to:
  #       1. for crop
  #       2. for noncrop
  
  # or if make_binary = TRUE, update to:
  #       0. for crop
  #       1. for noncrop
  

  # check if x, y columns
  if (length(grep("[xy]$", names(dt))) > 0) {
    if (!identical(names(dt)[1:2], c("x", "y"))) {stop("x and y must be the first two columns in the data.table")}
    
    for (x in names(dt[, 3:length(dt)])) { # can also use names(dt[, !c("x", "y")])
      set(dt, which(dt[[x]] == 0), x, NA)   # set 0 values to NA
    }        
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 1), x, NA)   # set nonveg (urban, water, etc.) to NA
    }
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 3), x, 1)    # set crop from 3 to 1
    }
    
    for (x in names(dt[, 3:length(dt)])) {
      set(dt, which(dt[[x]] == 4), x, 2)    # combine 4 (grassland) and 2 (woody)
      # into a single noncrop layer (2)
    }
  } else {
    
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
cc_make_binary <- function(dt) {
  
  if (length(grep("^[xy]$", names(dt))) > 0) {
    
    if (!identical(names(dt)[1:2], c("x", "y"))) {
      stop("x and y must be the first two columns in the data.table")
    }
    
    for (i in 3:length(dt)) {
      dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1] 
    
    }
  }
  
    else {
    for (i in seq_len(length(dt))) {
      dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1] 
    }
    }
}
# alternatively: dt <- dt - 1



# ---------------- Make diff -------------------------

cc_make_diff <- function(dt){
  dt_lead <- copy(dt)
  dt_lead[, names(dt_lead)[1] := NULL][, end := 0]
  dt_diff <- dt_lead - dt
}



# ---------------- Filter out all crop or all noncrop pixels -------------------------
cc_remove_nonabn <- function(dt) {
  dt[dt[, rowSums(.SD) > 0 & rowSums(.SD) < length(.SD)], ] # this is inefficient with memory, but since I'll be in adroit anyways, it'll probably be fine.
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
cc_erase_nonabn_periods <- function(dt) {
  
  # iterate across column indices
  for (i in seq_len(length(dt))) {
    dt[get(names(dt)[i]) == i,  # filter rows with values equal to column number
       names(dt)[i] := 0] # set value to 0
  }
  
}


# ---------------- Extract lengths of all abandonment periods -----------------------------

cc_extract_lengths <- function(dt) {
  abn_lengths <- vector(mode = "numeric")
  for(i in seq_len(length(dt))) {
    abn_lengths <- c(abn_lengths, 
                     dt[get(names(dt)[i]) < 0,
                        get(names(dt)[i])])
  }
  abs(abn_lengths)
}


# f_dowle2 = function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), (i):=0]
# }


