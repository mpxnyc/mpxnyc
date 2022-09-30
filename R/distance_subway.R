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
api_key = "AIzaSyCZsXIB8OhQRMRefBS2u0DGipM12mPLKw8"

  census_tract_from <- "36005043600"
  census_tract_to <- "36005028800"

from_coords <- census_tract_centroids |>
  dplyr::filter(GEOID == census_tract_from) |>
  dplyr::pull(coord)

to_coords <- census_tract_centroids |>
  dplyr::filter(GEOID == census_tract_to) |>
  dplyr::pull(coord)

endpoint <- stringr::str_interp("https://maps.googleapis.com/maps/api/directions/json?destination=${to_coords[2]},${to_coords[1]}&origin=${from_coords[2]},${from_coords[1]}&key=${api_key}")

}
