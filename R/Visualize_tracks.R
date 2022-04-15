# ---------------------------------------------------------------------------------------------------------------
#' Map logger tracks
#'
#' @description
#' Creates a simple plot of logger tracks using a grouping variable for plot colours
#'
#' @param data tracking data in the seabiRds format
#' @param groups name of the variable to use for plotting line/point colours
#' @param legend_label Title for legend
#' @param use_legend Should the legend be plotted (TRUE/FALSE)

mapTracks <- function(data, groups = 'dep_id', legend_label = 'Deployments', use_legend = T) {

  if (max(data$coldist, na.rm = T) < 500) {
    world <- rnaturalearth::ne_countries(scale = 50, returnclass = 'sf')
  } else {
    world <- rnaturalearth::ne_countries(scale = 110, returnclass = 'sf')
  }

  my_sf <- sf::st_as_sf(data[!is.na(data$lon),], coords = c('lon', 'lat'), crs = 4326)
  my_bb <- bbox_at_zoom(locs=my_sf)

  xran <- my_bb[c(1,3)]
  yran <- my_bb[c(2,4)]

  coord_range <- max(c(xran[2] - xran[1], yran[2] - yran[1]))
  coord_breaks <- 0.1
  if (coord_range > 0.5) coord_breaks <- 0.2
  if (coord_range >= 1) coord_breaks <- 0.5
  if (coord_range >= 2) coord_breaks <- 1
  if (coord_range >= 5) coord_breaks <- 2
  if (coord_range >= 10) coord_breaks <- 5

  suppressMessages(
    myMap <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world) +
      ggplot2::geom_point(data = data[!is.na(data$lon),],
                          ggplot2::aes_string(x = 'lon', y = 'lat', col = groups), show.legend = use_legend) +
      ggplot2::geom_path(data = data[!is.na(data$lon),],
                         ggplot2::aes_string(x = 'lon', y = 'lat', col = groups), show.legend = use_legend) +
      ggplot2::scale_x_continuous(breaks = seq(-180, 180, coord_breaks),
                                  expand = ggplot2::expansion(mult = c(.1, .1))) +
      ggplot2::scale_y_continuous(breaks = seq(-90, 90, coord_breaks),
                                  expand = ggplot2::expansion(mult = c(.1, .1))) +
      ggplot2::coord_sf(xlim = xran, ylim = yran) +
      ggplot2::theme_light() +
      ggplot2::labs( x = "", y = "", col = legend_label)
  )

  myMap
  #' @export mapTracks
}
