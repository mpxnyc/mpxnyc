#' Subway Distance
#'
#' @description Returns the shortest subway distance between a pair of census tracts
#'
#' @param census_tract_from Origin census tract
#' @param census_tract_to Destination census tract
#'
#' @return vector of distances
#' @export
#'
distance.subway <- function(census_tract_from, census_tract_to) {
  # https://maps.googleapis.com/maps/api/directions/json
  # ?destination=Montreal
  # &origin=Toronto
  # &key=YOUR_API_KEY
}
