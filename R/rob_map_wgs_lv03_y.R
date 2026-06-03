#' Convert WGS84 coordinates to Swiss LV03 northing (y)
#'
#' Applies the Swisstopo approximation formula to convert WGS84 latitude and
#' longitude to a Swiss LV03 northing. Use together with [rob_map_wgs_lv03_x()].
#'
#' @source <https://github.com/ValentinMinder/Swisstopo-WGS84-LV03>
#'
#' @param lat Numeric. WGS84 latitude in decimal degrees.
#' @param lon Numeric. WGS84 longitude in decimal degrees.
#' @return Numeric. LV03 northing (y-coordinate).
#' @export
#' @examples
#' rob_map_wgs_lv03_y(lat = 46.9481, lon = 7.4474)

rob_map_wgs_lv03_y <- function(lat, lon) {
  lat     <- rob_map_dezsex(lat)
  lon     <- rob_map_dezsex(lon)
  lat_aux <- (lat - 169028.66) / 10000
  lon_aux <- (lon - 26782.5)   / 10000
  200147.07 +
    308807.95 * lat_aux +
    3745.25   * (lon_aux^2) +
    76.63     * (lat_aux^2) -
    194.56    * (lon_aux^2) * lat_aux +
    119.79    * (lat_aux^3)
}
