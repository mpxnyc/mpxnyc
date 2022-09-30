google.maps.fetch.single <- function(census_tract_from = "36005036901", census_tract_to = "36005000400", mode_of_transport = "subway", parameter="distance"){

  api_key = "AIzaSyCZsXIB8OhQRMRefBS2u0DGipM12mPLKw8"

  from_coords <- census_tract_centroids |>
    dplyr::filter(GEOID == census_tract_from) |>
    dplyr::pull(coord)

  to_coords <- census_tract_centroids |>
    dplyr::filter(GEOID == census_tract_to) |>
    dplyr::pull(coord)

  endpoint <- stringr::str_interp("https://maps.googleapis.com/maps/api/directions/json?destination=${to_coords[2]},${to_coords[1]}&origin=${from_coords[2]},${from_coords[1]}&key=${api_key}&mode=${mode_of_transport}&units=metric")
  response <- tidyjson::spread_all(endpoint)

  result <- response$..JSON[[1]][["routes"]][[1]][["legs"]][[1]][[parameter]][["value"]]

  return(result)
}

slowly.google.maps.fetch.single <- function(...) "dummy"

.onLoad <- function(lib, pkg) {
  slowly.google.maps.fetch.single <<- purrr::slowly(google.maps.fetch.single, rate=purrr::rate_delay(pause=0.3))
}


#' Title
#'
#' @param census_tract_from
#' @param census_tract_to
#' @param mode_of_transport
#' @param parameter
#'
#' @return
#' @export
#'
google.maps.fetch <- function(census_tract_from = sample(census_tract_centroids$GEOID, 20), census_tract_to = sample(census_tract_centroids$GEOID, 20),  mode_of_transport = "subway", parameter="distance"){
  purrr::map2_int(census_tract_from, census_tract_to, function(x,y) google.maps.fetch.single(x, y, mode_of_transport=mode_of_transport, parameter=parameter))
}
