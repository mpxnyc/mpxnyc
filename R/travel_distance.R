#' Travel Distance
#'
#' @param census_tract_from Origin Census Tract (string)
#' @param census_tract_to Destination Census Tract (string)
#' @param mode_of_transport Mode of transport (string) "subway" or "drive" or "walk"
#' @param key Google Maps API Key
#'
#' @return data.frame with columns "distance" (in meters) and "duration" (in seconds)
#' @export
#'
travel_distance <- function(census_tract_from, census_tract_to, mode_of_transport, ...) {
  purrr::map2(census_tract_from, census_tract_to, function(x,y) travel_distance_single(census_tract_from = x, census_tract_to = y, mode_of_transport=mode_of_transport, ...)) |>
    dplyr::bind_rows() |>
    data.frame()
}

travel_distance_single_fast <- function(mode_of_transport, ...) {
  if (mode_of_transport == "subway") return(distance.subway(...))
  if (mode_of_transport == "drive") return(distance.drive(...))
  if (mode_of_transport == "walk") return(distance.walk(...))
}

travel_distance_single <- function(...) "dummy"

.onLoad <- function(lib, pkg) {
  travel_distance_single <<- purrr::slowly(travel_distance_single_fast, rate=purrr::rate_delay(pause=0.3))
}


