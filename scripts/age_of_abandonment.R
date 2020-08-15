# Chris's play code, July 30th, 2020
# assigning age of pixels, and length of time abandoned

library(data.table)

# ---------------------------------------------------------------------------
# make your raster, data.table, etc.
# ---------------------------------------------------------------------------
set.seed(34L)
dt <- matrix(round(runif(15*15)),
             nrow = 15, ncol = 15) %>%
  as.data.frame()
setDT(dt)
# dt <- as.data.table(dt) # also works, but is less efficient because it creates a copy in memory. Not a big deal either way though.

# take a look at the data.table
# when more than 100 rows, print(dt) automatically just shows the first 5 and last 5 rows, which is handy.
dt


# update the names
names(dt) <- paste0("y", 1:15)



# ---------------------------------------------------------------------------
# calculate age of each pixel, for just one column
# ---------------------------------------------------------------------------
dt[y2 > 0, y2 := y1 + 1][]

# this code does the following:
# take dt, subset rows where y2 > 0, and 
# then set those equal to the value in column y1 for that row + 1.
# Note: the [] at the end prints the result.

# in order to loop over the columns, the code needs some massaging to get it to accept
# character vectors, which can be referenced with indices to be looped over
dt[get(names(dt)[2]) > 0, c(names(dt)[2]) := get(names(dt)[1]) + 1][]


# ---------------------------------------------------------------------------
# full implementation with for loop
# ---------------------------------------------------------------------------
for (i in 2:ncol(dt)) {
  
  # subset rows that are greater than 0 (i.e. 1, for noncrop), and
  dt[get(names(dt)[i]) > 0, 
     
     # set them equal to the previous column's value in that row, plus 1.
      names(dt)[i] := get(names(dt)[i-1]) + 1] 
}

print(dt)















# ---------------------------------------------------------------------------
# old code
# ---------------------------------------------------------------------------
# You'll note that this code works on each column.
# My previous code worked on individual values in
# each row individually, then looping over the columns, 
# then over all the rows. This ultimately ended up being
# much, much slower. I realized that data.tables could be 
# subset up front based on values in a column, and then 
# those values could be easily manipulated. 
# It turns out it's much easier to iterate over columns 
# than over rows, especially when we have something 
# like 90 million rows!

# the old code looked like this:
for (i in 1:nrow(dt)) {
  for (j in 2:ncol(dt)) {
    if (dt[i][[names(dt)[j]]] > 0) {
      dt[i][[names(dt)[j]]] <- dt[i][[names(dt)[j - 1]]] + 1
    }
  }
}
