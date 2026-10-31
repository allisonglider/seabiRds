# ---------------------------------------------------------------------------------------------------------------
#' Map logger tracks
#'
#' @description
#' Creates a simple plot of logger tracks using a grouping variable for plot colours
#'
#' @param data tracking data in the seabiRds format. This can be a data frame from the standardized
#' gps dataset, for example \code{dplyr::collect(arrow::open_dataset('path/gps'))}. Locations are
#' sorted by dep_id and time (if these columns are present) before they are drawn.
#' @param groups name of the variable to use for plotting line/point colours
#' @param legend_label Title for legend
#' @param use_legend Should the legend be plotted (TRUE/FALSE)
#' @param deployed_only If TRUE and data has a deployed column, only locations collected while the logger
#' was deployed (deployed = 1) are mapped. The standardized datasets keep locations before release and
#' after recapture, flagged with deployed = 0. Default is FALSE.
#'
#' @details
#' The level of detail in the base map depends on how far the locations are from the colony. If data has a
#' coldist column (distance from the colony, in km) it is used. If not, the distance from the first location
#' is used.
#' @export

mapTracks <- function(data, groups = 'dep_id', legend_label = 'Deployments', use_legend = T,
                      deployed_only = FALSE) {

  data <- as.data.frame(dplyr::ungroup(data))

  if (deployed_only) {
    if ('deployed' %in% names(data)) {
      data <- data[!is.na(data$deployed) & data$deployed == 1, ]
      if (nrow(data) == 0) stop('There are no deployed locations to map', call. = FALSE)
    } else {
      warning('data has no deployed column, so all locations are mapped', call. = FALSE)
    }
  }

  # locations are joined in the order they appear, so make sure they are in time order for each deployment
  if ('time' %in% names(data)) {
    if ('dep_id' %in% names(data)) data <- data[order(data$dep_id, data$time), ] else data <- data[order(data$time), ]
  }

  data <- data[!is.na(data$lon), ]

  # the distance from the colony is used to choose the detail of the base map
  if ('coldist' %in% names(data)) {
    max_dist <- max(data$coldist, na.rm = T)
  } else {
    max_dist <- max(getColDist(lon = data$lon, lat = data$lat, colonyLon = data$lon[1], colonyLat = data$lat[1]), na.rm = T)
  }

  if (max_dist < 500) {
    world <- rnaturalearth::ne_countries(scale = 50, returnclass = 'sf')
  } else {
    world <- rnaturalearth::ne_countries(scale = 110, returnclass = 'sf')
  }

  my_sf <- sf::st_as_sf(data, coords = c('lon', 'lat'), crs = 4326)
  my_bb <- bbox_at_zoom(locs=my_sf)

  xran <- my_bb[c(1,3)]
  yran <- my_bb[c(2,4)]

  suppressMessages(
    myMap <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world) +
      ggplot2::geom_point(data = data,
                          ggplot2::aes(x = lon, y = lat, col = if (is.null(groups)) NULL else .data[[groups]]),
                          show.legend = use_legend) +
      ggplot2::geom_path(data = data,
                         ggplot2::aes(x = lon, y = lat, col = if (is.null(groups)) NULL else .data[[groups]]),
                         show.legend = use_legend) +
      ggplot2::coord_sf(xlim = xran, ylim = yran) +
      ggplot2::theme_light() +
      ggplot2::labs( x = "", y = "", col = legend_label)
  )

  myMap
}
