#

amazon <- raster("/Users/christophercrawford/Downloads/AMAZONIA.tif") # defaults tto band 1
amazon_1985 <- raster("/Users/christophercrawford/Downloads/AMAZONIA.tif", band = 1)
plot(amazon)
amazon@file@nbands

amazon_ts <- stack("/Users/christophercrawford/Downloads/AMAZONIA.tif")
str(amazon_ts)
amazon_ts@layers[[3]]

amazon
values(amazon)

amazon_br <- brick("/Users/christophercrawford/Downloads/AMAZONIA.tif")
amazon_br
values(amazon_br)
amazon_br@data
ncell(amazon_br)

hist(amazon_1985, xlim = c(0, 30))
dev.off()
hist(amp_list4$all_richness)

