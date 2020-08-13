# ---------------------------------------------------------------
#
# data.table functions
# 
# ---------------------------------------------------------------


# -------------------------
# Update land cover classes values
# -------------------------

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



# 
cc_make_binary <- function(dt) {
    for (i in seq_len(length(dt))) {
      dt[, c(names(dt)[i]) := get(names(dt)[i]) - 1] 
    }
  }




# -------------------------
# Calculate age of each noncrop cell
# -------------------------
# write function
calc_age <- function(dt) {
  for (i in 2:ncol(dt)) {
    
    # subset rows that are greater than 0 (i.e. 1, for noncrop), and
    dt[get(names(dt)[i]) > 0, 
       
       # set them equal to the previous column's value in that row, plus 1.
       c(names(dt)[i]) := get(names(dt)[i-1]) + 1] 
  }
}





# -------------------------
# 
# -------------------------






# -------------------------
# 
# -------------------------





# -------------------------
# 
# -------------------------





# -------------------------
# 
# -------------------------

