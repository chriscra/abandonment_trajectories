# Fasterizing reptilian AOH polygons

library(sf)
library(raster)

AOH <- st_read("/Users/christophercrawford/Google Drive/_Projects/data/Bd/AOH/reptilia_AOH/reptilia.shp")

# check it out
AOH %>% 
  filter(binom == "Archaius_tigris") %>% 
  st_geometry() %>% 
  plot()

AOH %>% 
  st_geometry() %>% 
  plot(add = TRUE)
# tiny lil guys, eh?



# -------------------
# First, make a template raster

mask <- raster()
res(mask) <- 0.01


# values(mask) <- 0 # If you want, you can add values to the raster. You don't have to though, and it just makes the raster HUGE at this resolution (4.8 GB)
# writeRaster(mask, "mask.tif") # if you add values, you'll need to just write it to your machine, otherwise you'll quickly run out of memory, as the fasterize layer will be the same size.  


# -------------------
#  Fasterize: 

AOH_r <- fasterize(AOH, mask, fun = "sum") # field = NULL by default, which gives every polygon a value of 1

plot(AOH_r) # again.... tiny lil guys!




# -------------------
# Notes:

# If you aren't already familiar, these are great packages for basemaps:
install.packages("rnaturalearth")
devtools::install_github("ropensci/rnaturalearthdata")
devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

plot(ne_countries())  # nice!
# plot(ne_states()) # or all sub national level bodies with this one 



# if you need to reproject the mask before you fasterize, I think raster works best with: 
crs(AOH) # note, seems to work better than st_crs().

# in the event that you want to build a raster based on a smaller existing one
template_raster <- extend(small_mask, extent(as.Spatial(sf_file)), value = 0)


