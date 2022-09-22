#' Travel Time
#'
#' @param census_tract_from Origin census tract
#' @param census_tract_to Destination census tract
#' @param mode_of_transport Which mode of transport. Default is "subway", other options are "walk" and "drive"
#'
#' @return
#' @export
#'
#' @examples
travel_time <- function(..., mode_of_transport = "subway") {
  if (mode_of_transport == "subway") return(subway_time(...))
  if (mode_of_transport == "drive") return(drive_time(...))
  if (mode_of_transport == "walk") return(walk_time(...))
}
