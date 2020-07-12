
library(raster)

# Create test matrices with value of 1 for agriculture and 2 for non-agriculture
mat1 <- matrix(rep(1, 9), nrow = 3, ncol = 3)
mat2 <- matrix(c(rep(1, 3), 2, 2, 1), nrow = 3, ncol = 3)
mat3 <- matrix(c(rep(1, 3), 2, 1, 1), nrow = 3, ncol = 3)

# Create raster stack, with one raster for each year
rastStack <- stack(raster(mat1),
                   raster(mat2),raster(mat2),
                   raster(mat1),
                   raster(mat2), raster(mat3), 
                   raster(mat1))
# Name the years
yrs <- 1:nlayers(rastStack)
names(rastStack) <- paste("yr", yrs, sep = "")

# Have a look
# yr2 -> two cells change to non-agriculture
# yr4 -> the two cells change back to agriculture
# yr5 -> the same two cells change to non-agriculture
# yr6 -> one of the two cells changes back to agriculture
# yr7 -> the other cell also changes back to agriculture
plot(rastStack)

# Sequential difference between years
# value of 0 = no difference compared to previous year
# value of 1 = change from agriculture to non-agriculture
# value of -1 = change from non-agriculture to agriculture
running_diff <- calc(rastStack, fun = diff)

# Multiply by the year to record the year in which change took place
running_diff2 <- running_diff * yrs[-1]

# Identify years with zero change
minVals <- minValue(running_diff2)
maxVals <- maxValue(running_diff2)
nochange <- running_diff2[[which(maxVals == 0 & minVals == 0)]]

# For each cell, we now know the year in which it changed from agriculture to 
# non-agriculture and (potentially) back to agriculture, from which we can calculate
# the number of non-agriculture periods and total time as non-agriculture

# Number non-agriculture periods = number years (bands) where cell value is positive
noncrop_count <- sum(running_diff2 > 0)
# Number agriculture reversions = number years (bands) where cell value is negative
crop_count <- sum(running_diff2 < 0)

# Convert original stack to binary -> 1 if non-agriculture, 0 if agriculture
rastStack_bi <- rastStack
rastStack_bi[rastStack_bi == 1] <- 0
rastStack_bi[rastStack_bi == 2] <- 1
# Total time as non-agriculture = sum across years
noncrop_duration <- calc(rastStack_bi, fun = sum)

# Average length of time as non-agriculture = total duration / N non-crop periods
noncrop_duration_avg <- noncrop_duration / noncrop_count
plot(noncrop_duration_avg)

# cell 2 [1,2] changes from agriculture in 2001, back to agriculture in 2003 = 2 years
# cell 2 [1,2] changes from agriculture in 2004, back to agriculture in 2006 = 2 years
# cell 2 [1,2] total time = 4 years; avg time = 4 years / 2 periods = 2 years

# cell 5 [2,2] changes from agriculture in 2001, back to agriculture in 2003 = 2 years
# cell 5 [2,2] changes from agriculture in 2004, back to agriculture in 2005 = 1 year
# cell 5 [2,2] total time = 3 years; avg time = 3 years / 2 periods = 1.5 years




