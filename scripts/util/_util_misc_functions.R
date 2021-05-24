# --------------------------------------------------------------- #
#
# Miscellaneous functions
# 
# --------------------------------------------------------------- #


# ------------------------- #
# Update packages
# ------------------------- #

#update.packages()


# ------------------------- #
# calculate the sizes of items in the environment
# ------------------------- #
env_size <- function(workspace = ls()) {
  size = 0
  for (x in workspace){
    thisSize = object_size(get(x))
    size = size + thisSize
    message(x, " = ", appendLF = F); print(thisSize, units='auto')
  }
  message("total workspace is ", appendLF = F); print(size, units='auto')
}

# ------------------------- #
# calculate a dummy data.table
# ------------------------- #
cc_create_dt <- function(numrow = 15, numcol = 15, seed = 34L) {
  set.seed(seed)
  dt <- matrix(round(runif(numrow*numcol)), nrow = numrow, ncol = numcol)
  dt <- as.data.frame(dt)
  setDT(dt)
}

# ------------------------- #
#
# ------------------------- #
cc_create_bin <- function(numrow = 15, numcol = 15, seed = 34L) {
  dt <- cc_create_dt(numrow = numrow, numcol = numcol, seed = seed)
  dt[3] <- 1
  dt[13] <- 1
  dt[12] <- 0
  dt[4, 1:4] <- 1
  dt[14, 1] <- 1
  dt[1, 9] <- 1
  dt[3, 1:2] <- 0
  dt
}


# ------------------------- #
# capitalize name
# ------------------------- #
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(
      toupper(substring(s, 1, 1)), 
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), 
         cap, 
         USE.NAMES = !is.null(names(s))
  )
}


# ------------------------ #
# capitalize site labels
# ------------------------ #
cap_labels <- function(string) capwords(gsub("_", " ", string))
# for use in facet_wrap(labeller = as_labeller(cap_labels))


# ------------------------ #
# capitalize site labels
# ------------------------ #
cap_update_labels <- function(string) {
  capwords(ifelse(grepl("herze", string, ignore.case = TRUE),
                  gsub("_", " & ", string),
                  ifelse(grepl("bel", string, ignore.case = TRUE),
                         gsub("belarus", "Belarus/Smolensk", string),
                         ifelse(grepl("goias", string, ignore.case = TRUE),
                                gsub("goias", "Goiás", string),
                                ifelse(grepl("shaanxi", string, ignore.case = TRUE),
                                       gsub("shaanxi", "Shaanxi/Shanxi", string),
                                       gsub("_", " ", string))))))
}
# for use in facet_wrap(labeller = as_labeller(cap_update_labels))



# ------------------------ #
# scale a vector by mean and sd
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
