#' Look up a building in the Swiss federal building register (GWR)
#'
#' Fetches building characteristics from the swisstopo REST API for a given
#' federal building identifier (EGID) and returns the result as a data frame.
#'
#' @param egid Integer or character. Federal building identifier (EGID).
#' @return A data frame with the building's GWR attributes.
#' @import rvest
#' @export
#' @examples
#' \dontrun{
#' rob_rev_gwr(egid = 1234567)
#' }

rob_rev_gwr <- function(egid) {
  stopifnot(is.numeric(egid) || is.character(egid), length(egid) == 1L)
  link <- paste0(
    "https://api.geo.admin.ch/rest/services/ech/MapServer/",
    "ch.bfs.gebaeude_wohnungs_register/",
    egid, "_0/extendedHtmlPopup?lang=de"
  )
  page <- read_html(link)
  page |> html_element("table") |> html_table()
}
