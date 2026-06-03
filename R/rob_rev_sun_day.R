#' Compute per-minute sun/shadow status at a Swiss location
#'
#' For every minute of a given day, calculates the sun's altitude and azimuth
#' and checks whether direct sunlight is blocked by surrounding terrain. The
#' check works by casting rays from the observer location toward the sun,
#' sampling elevation along each ray, and comparing the terrain angle against
#' the solar altitude angle.
#'
#' @param location Numeric vector of length 2: `c(lat, lon)` in WGS84 decimal
#'   degrees. Default is Buechli, Switzerland.
#' @param day_of_year Integer. Day of the year (1 = 1 January, 365 = 31 December).
#' @param ray_length Numeric. Radius of the terrain scan in metres (default 10 000 m).
#' @param ray_intervals Numeric. Distance between sampled elevation points along
#'   each ray in metres (default 100 m). Smaller values are more accurate but slower.
#' @param zoom_level Integer. Zoom level for the AWS elevation tiles (1–15).
#'   Higher values are more accurate but significantly slower (default 12).
#' @return A tibble with 1 440 rows (one per minute of the day) and columns:
#'   \describe{
#'     \item{time}{`hms` time of day.}
#'     \item{azimut}{Solar azimuth in degrees.}
#'     \item{alpha}{Solar altitude in degrees.}
#'     \item{sun_in_min}{1 = direct sunlight, 0 = in shadow or below horizon.}
#'   }
#' @import solrad elevatr
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr arrange bind_cols group_by mutate select summarise ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_replace
#' @importFrom hms as_hms
#' @export
#' @examples
#' \dontrun{
#' # Leukerbad Therme on 29 June (day 180)
#' data <- rob_rev_sun_day(c(46.378672, 7.629984), day_of_year = 180,
#'                         ray_length = 1000, ray_intervals = 10,
#'                         zoom_level = 12)
#'
#' # Plot sun/shadow and altitude over the day
#' hour_major <- as_tibble(seq(1:11))
#' time_hour_major <- hms::as_hms(hour_major$value * 60 * 60 * 2)
#'
#' hour_minor <- as_tibble(seq(1:24))
#' time_hour_minor <- hms::as_hms(hour_minor$value * 60 * 60)
#'
#' data |>
#'   dplyr::filter(alpha > 0) |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_line(ggplot2::aes(x = time, y = sun_in_min), linewidth = 2) +
#'   ggplot2::geom_line(ggplot2::aes(x = time, y = alpha / 30),
#'                      linewidth = 2, color = "red") +
#'   ggplot2::labs(
#'     title = "Tagesverlauf der Sonne in Leukerbad Therme",
#'     x = "Uhrzeit (Winterzeit)",
#'     y = "Schatten / Sonne (schwarz)"
#'   ) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::scale_x_time(
#'     breaks       = time_hour_major,
#'     minor_breaks = time_hour_minor,
#'     limits       = c(min(time_hour_minor), max(time_hour_minor))
#'   ) +
#'   ggplot2::scale_y_continuous(
#'     sec.axis = ggplot2::sec_axis(~ . * 30, name = "Alpha (rot)")
#'   )
#' }

rob_rev_sun_day <- function(
    location      = c(46.64229346436842, 7.443874357959411),
    day_of_year   = 37,
    ray_length    = 10000,
    ray_intervals = 100,
    zoom_level    = 12
) {

  coord_0 <- location

  # ---- Solar angles --------------------------------------------------------
  # Build a sequence of fractional day-of-year values, one per minute
  DOY <- seq(day_of_year, day_of_year + 1 - (1 / 1440), 1 / 1440)

  alpha   <- as_tibble(solrad::Altitude(DOY, Lat = coord_0[1], Lon = coord_0[2],
                                         SLon = coord_0[2], DS = 0))
  azimuth <- as_tibble(solrad::Azimuth( DOY, Lat = coord_0[1], Lon = coord_0[2],
                                         SLon = coord_0[2], DS = 0))

  # ---- Elevation at observer -----------------------------------------------
  # Metres per degree of lat/lon at this latitude (WGS84 approximation)
  metres_per_deg_lon <- 111300 * cos(coord_0[1] * pi / 180)
  metres_per_deg_lat <- 111300

  observer_df   <- data.frame(x = coord_0[2], y = coord_0[1])
  observer_elev <- elevatr::get_elev_point(observer_df, prj = 4326,
                                           src = "aws", z = zoom_level)$elevation

  # ---- Build terrain-ray table ---------------------------------------------
  # Each minute gets one ray pointing from the observer toward the sun.
  # ray_points elevation samples are taken along that ray.
  ray_points <- ray_length / ray_intervals

  # Per-coordinate step size along a ray for one interval (in decimal degrees)
  rays <- tibble(
    azimut = azimuth$value,
    alpha  = alpha$value,
    lat    = coord_0[1],
    long   = coord_0[2]
  ) |>
    pivot_longer(cols = c(lat, long), names_to = "key", values_to = "value") |>
    arrange(azimut) |>
    mutate(
      azimut = (azimut + 180) * (pi / 180),   # shift: ray goes *toward* sun
      alpha  = alpha * (pi / 180),
      # lat step: positive cos component; lon step: negative sin component
      add = ifelse(
        key == "long",
        -(sin(azimut) * ray_intervals) / metres_per_deg_lon,
         (cos(azimut) * ray_intervals) / metres_per_deg_lat
      )
    )

  # Expand: one row per (ray × sample point)
  point_placeholder <- as_tibble(matrix(0, nrow = 2 * 1440, ncol = ray_points))
  rays <- bind_cols(rays, point_placeholder)

  rays <- rays |>
    pivot_longer(cols = seq(6, ncol(rays)), names_to = "point", values_to = "value2") |>
    mutate(point = as.numeric(str_replace(point, "V", ""))) |>
    arrange(azimut) |>
    select(-value2) |>
    group_by(azimut, key) |>
    mutate(add2 = cumsum(add)) |>    # cumulative offset = distance along ray
    ungroup() |>
    mutate(point_coord = value + add2) |>
    select(azimut, alpha, point, key, point_coord) |>
    pivot_wider(names_from = key, values_from = point_coord) |>
    arrange(azimut, point)

  # ---- Fetch terrain elevation for all sample points -----------------------
  rays_points_df <- rays |> select(x = long, y = lat) |> as.data.frame()
  rays_elev_sf   <- elevatr::get_elev_point(rays_points_df, prj = 4326,
                                             src = "aws", z = zoom_level)
  rays_elev <- bind_cols(rays, as_tibble(rays_elev_sf$elevation))

  # ---- Shadow check --------------------------------------------------------
  # A sample point blocks sunlight when its terrain angle exceeds the solar
  # altitude (alpha). If any point on a ray is blocking, sun_in_min = 0.
  rays_sun_check <- rays_elev |>
    mutate(
      elevation_diff       = value - observer_elev,
      distance_horizontal  = sqrt(
        ((long - coord_0[2]) * metres_per_deg_lon)^2 +
        ((lat  - coord_0[1]) * metres_per_deg_lat)^2
      ),
      terrain_angle = atan(elevation_diff / distance_horizontal),
      # 0 when sun is below horizon or blocked by terrain
      sun = ifelse(alpha < 0, 0L, ifelse(alpha < terrain_angle, 0L, 1L))
    )

  rays_sun_min <- rays_sun_check |>
    group_by(azimut) |>
    summarise(
      alpha      = min(alpha),
      sun_in_min = ifelse(min(sun) == 0, 0L, 1L)
    )

  # ---- Attach clock time ---------------------------------------------------
  minute_index <- as_tibble(seq_len(nrow(rays_sun_min)))
  clock_time   <- as_tibble(as_hms(minute_index$value * 60))

  rays_sun_min |>
    mutate(
      time   = clock_time$value,
      azimut = azimut * (180 / pi),
      alpha  = alpha  * (180 / pi)
    ) |>
    select(time, azimut, alpha, sun_in_min)
}
