# ---------------------------------------------------------------
#
# Spatial Functions: 
# 
# ---------------------------------------------------------------


invert = function(x){
  return(1-x)}

normalize <- function(x) {
  (x - cellStats(x,"min")) / (cellStats(x,"max") - cellStats(x,"min"))
}

# --------------- gdal_polygonizeR function -----------------

# Originally written by Lyndon Estes (Clark University), and 
# expanded upon by John Baumgartner:
# https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- "/Users/christophercrawford/Google Drive/_Projects/Zambia/agroEcoTradeoff/python/gdal_polygonize.py" #Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

check_crs <- function (x, y) {
  x_crs <- if (class(x)[1] == "sf") {st_crs(x)$proj4string} else {proj4string(x)}
  y_crs <- if (class(y)[1] == "sf") {st_crs(y)$proj4string} else {proj4string(y)}
  if (x_crs == y_crs) {
    "Match: the two projections are the same. Woot!"
  } else {
    "Don't Match: the two projections are not the same. Try reprojecting."
    }
}

cc_write_reload_raster <- function(raster, name, directory) {
  # This function saves and reloads a raster,
  # 1. sets the layer name of that raster to the input character vector "name"
  # 2. creates the file path from that name
  # 3. writes the raster to that filepath
  # 4. Finally, reloads the raster

  names(raster) <- name
  #names(named_raster) <- deparse(quote(named_raster)) # failed usage
  path <- paste0(directory, .Platform$file.sep, names(raster),".tif")
  writeRaster(raster, path, overwrite=TRUE)
  raster(path)
}

# glwd_africa_r <- cc_write_reload_raster(glwd_africa_r, "glwd_africa_r", p_basemaps)



cc_make_valid <- function (sf, add_reasons = TRUE) {
  if (class(sf)[1] != "sf") {stop("The function requires the input to be an sf object.")}# 0. Check to make sure input is an sf object.
  if (add_reasons == FALSE) {
    sf <- st_make_valid(sf) # 2a. run st_make_valid to make the geometries valid. This usually takes a long time.
    return(sf)
  } else {
    sf <- mutate(sf, pre_fix_reasons = st_is_valid(sf, reason = TRUE))
    # 1. Add new column listing whether each geometry is valid or not (along with reasons why not; NA means corrupt geometry)

    sf <- st_make_valid(sf)
    # 2a. run st_make_valid to make the geometries valid. This usually takes a long time.

    sf <- mutate(sf, post_fix_reasons = st_is_valid(sf, reason = TRUE))
    # 3. Check geometries again, and add another column to reasons_df

    return(sf)
  }
}


cc_rescale <- function(raster, factor, run_mask = TRUE, mask, fun = mean) {
  raster_rescaled <- raster::aggregate(raster, fact = factor, fun = fun)
  raster_output <- disaggregate(raster_rescaled, fact = factor)

  if (run_mask) {
    raster_output_masked <- raster_output %>%
      crop(extent(mask)) %>%
      raster::mask(mask)
    raster_output_masked
  } else {
    raster_output
  }
}


cc_write_bd_to_dt <- function(bd_input, input_key) {
  bd_input_dt <- as.data.table(bd_input, xy = FALSE, keep.rownames=FALSE)
  names(bd_input_dt) <- "cons.priorities"
  fwrite(na.omit(bd_input_dt), # must omit all NA values
         file = paste0(p_dat, .Platform$file.sep,
                       input_key, .Platform$file.sep,
                       input_key, "-cons-priorities.csv"))
}



