#' Extracts data values from a raster stack or raster birck, where layers correpond to a time series
#'
#' @param raster_stack A stack or raster brick of environmental data, created using the 'raster' package
#' @param locations A spatial points dataframe or simple features multipoints object.
#' @param names_var A character string giving the name of the variable in locations that matches the layer names in raste_stack.
#'
#' @details
#' This function extends the raster::extract() function to extract data from a
#' RasterStack or RasterBrick. A stack or brick has a third dimension of raster
#' layers, in addition to the x-y coordinates.
#'
#' To use this function convert your spatial point location data to a spatial object class,
#' either a SpatialPointsDataFrame (sp package) or a Simple Features object (sf package).
#'
#' Add a new column to your spatial object that matches the layer names in your
#' raster data. Usually, the raster stack/brick layer names are based on dates and
#' you can create a matching column using base::strftime() to make a new vector with a specifid format.
#'
#' The function will loop through layers in your raster file extracting environmental data
#' for all locations that match that layer name.
#'
#' @return A vector of values extracted from the raster_stack, of the same length as the number of rows in locations.
#' @examples
#'
#' # Create example raster stack
#' r <- expand.grid(
#'   lon = seq(-10, 10, 1),
#'   lat = seq(-10, 10, 1),
#'   X2020.01.01 = 1,
#'   X2020.01.02 = 2
#' )
#' r <- raster::rasterFromXYZ(xyz = r, crs = '+proj=longlat')
#' r
#'
#' # Create random location data
#' locs <- data.frame(
#'   lon = runif(20, -9, 9),
#'   lat = runif(20, -9, 9),
#'   time = seq.POSIXt(as.POSIXct('2020-01-01 12:00:00'), as.POSIXct('2020-01-02 12:00:00'),
#'                     length.out = 20)
#' )
#'
#' # Add column with of values with names that match layers in stack
#' names(r)
#' locs$rn <- strftime(locs$time, 'X%Y.%m.%d')
#' intersect(locs$rn, names(r))
#'
#' # Extract values
#' locs <- SpatialPointsDataFrame(coords = locs[,c('lon','lat')],
#'                                data = locs, proj4string = CRS('+proj=longlat'))
#'
#' extract_raster_stack(raster_stack = r, locations = locs, names_var = 'rn')

extract_raster_stack <- function(raster_stack, locations, names_var) {

  if (!(class(raster_stack) %in% c("RasterStack", "RasterBrick"))) {
    stop("raster_stack must be either a RasterStack or RasterBrick", call. = F)
  }

  if (class(locations)[1] == 'sf') {
    locations <- as(locations, 'Spatial')
  }

  if (!(class(locations) %in% c("SpatialPointsDataFrame"))) {
    stop("locations must be a SpatialPointsDataFrame", call. = F)
  }

  output <- rep(NA, nrow(locations))
  loc_dates <- as.data.frame(locations)[,names_var]

  check_names <- setdiff(loc_dates, names(raster_stack))
  if (length(check_names) == length(unique(loc_dates))) {
    stop('Values in raster_names do not match raster layer names', call. = F)
  }

  if (length(check_names) > 0) {
    warning('Not all locations had a matching date in raster stack', call. = F)
  }

  for (i in names(raster_stack)) {
    idx <- which(loc_dates == i)
    output[idx] <- raster::extract(raster_stack[[i]], locations[idx,])
  }

  output

  #' @export extract_raster_stack
}
