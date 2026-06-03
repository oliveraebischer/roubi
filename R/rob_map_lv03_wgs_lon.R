#' Convert Swiss LV03 coordinates to WGS84 longitude
#'
#' Applies the Swisstopo approximation formula to convert a Swiss LV03 easting
#' and northing to WGS84 longitude. Use together with [rob_map_lv03_wgs_lat()].
#'
#' @source <https://github.com/ValentinMinder/Swisstopo-WGS84-LV03>
#'
#' @param x Numeric. LV03 easting (x-coordinate).
#' @param y Numeric. LV03 northing (y-coordinate).
#' @return Numeric. WGS84 longitude in decimal degrees.
#' @export
#' @examples
#' rob_map_lv03_wgs_lon(x = 600000, y = 200000)

rob_map_lv03_wgs_lon <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))
  x_aux <- (x - 600000) / 1000000
  y_aux <- (y - 200000) / 1000000
  lon <- 2.6779094 +
    4.728982 * x_aux +
    0.791484 * x_aux * y_aux +
    0.1306   * x_aux * (y_aux^2) -
    0.0436   * (x_aux^3)
  lon * 100 / 36
}
