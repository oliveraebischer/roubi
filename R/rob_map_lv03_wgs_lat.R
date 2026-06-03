#' Convert Swiss LV03 coordinates to WGS84 latitude
#'
#' Applies the Swisstopo approximation formula to convert a Swiss LV03 easting
#' and northing to WGS84 latitude. Use together with [rob_map_lv03_wgs_lon()].
#'
#' @source <https://github.com/ValentinMinder/Swisstopo-WGS84-LV03>
#'
#' @param x Numeric. LV03 easting (x-coordinate).
#' @param y Numeric. LV03 northing (y-coordinate).
#' @return Numeric. WGS84 latitude in decimal degrees.
#' @export
#' @examples
#' rob_map_lv03_wgs_lat(x = 600000, y = 200000)

rob_map_lv03_wgs_lat <- function(x, y) {
  x_aux <- (x - 600000) / 1000000
  y_aux <- (y - 200000) / 1000000
  lat <- 16.9023892 +
    3.238272  * y_aux -
    0.270978  * (x_aux^2) -
    0.002528  * (y_aux^2) -
    0.0447    * (x_aux^2) * x_aux -
    0.0140    * (y_aux^3)
  lat * 100 / 36
}
