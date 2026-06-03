#' Convert WGS84 coordinates to Swiss LV03 easting (x)
#'
#' Applies the Swisstopo approximation formula to convert WGS84 latitude and
#' longitude to a Swiss LV03 easting. Use together with [rob_map_wgs_lv03_y()].
#'
#' @source <https://github.com/ValentinMinder/Swisstopo-WGS84-LV03>
#'
#' @param lat Numeric. WGS84 latitude in decimal degrees.
#' @param lon Numeric. WGS84 longitude in decimal degrees.
#' @return Numeric. LV03 easting (x-coordinate).
#' @export
#' @examples
#' rob_map_wgs_lv03_x(lat = 46.9481, lon = 7.4474)

rob_map_wgs_lv03_x <- function(lat, lon) {
  lat     <- rob_map_dezsex(lat)
  lon     <- rob_map_dezsex(lon)
  lat_aux <- (lat - 169028.66) / 10000
  lon_aux <- (lon - 26782.5)   / 10000
  600072.37 +
    211455.93 * lon_aux -
    10938.51  * lon_aux * lat_aux -
    0.36      * lon_aux * (lat_aux^2) -
    44.54     * (lon_aux^3)
}
