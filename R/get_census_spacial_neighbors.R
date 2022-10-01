#' Get census spacial neighbors
#'
#' @description Find the neighboring census tracts for a vector of census tracts
#'
#' @return List of neighbor sets
#' @export
#'

get_census_spacial_neighbors <- function(census_tracts) {
    adj_matrix[census_tracts]
  }
