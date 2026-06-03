#' Convert decimal degrees to total arc-seconds
#'
#' Splits a decimal-degree angle into integer degrees, integer minutes, and
#' fractional seconds, then sums them as arc-seconds. Vectorised over `angle`.
#' Used internally by the WGS84 <-> LV03 coordinate conversion functions.
#'
#' @param angle Numeric. Angle in decimal degrees.
#' @return Numeric. Equivalent angle expressed as total arc-seconds.
#' @noRd

rob_map_dezsex <- function(angle) {
  deg  <- trunc(angle)
  mins <- trunc((angle - deg) * 60)
  sec  <- ((angle - deg) * 60 - mins) * 60
  sec + mins * 60 + deg * 3600
}
