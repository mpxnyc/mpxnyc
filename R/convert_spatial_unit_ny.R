#' Convert Spatial Units in New York City
#'
#' @description Convert to parent spatial unit in hierarchy: borough > community > neighborhood > census_tract
#'
#' @param input_census_tract Vector (character) of input census tracts to convert to neighborhoods or boroughs or community districts
#' @param input_neighborhood_name Vector (character) of input neighborhoods to convert to neighborhoods or boroughs
#' @param input_community_district Vector (character) of input communities to convert to boroughs
#' @param convert_to String showing how to convert census_tracts. Options are "neighborhood" (default), "community", and "borough"
#'
#' @return Vector (character) of neighborhood or borrough names
#' @export
#'
convert_spatial_unit_ny <- function(input_census_tract = NULL, input_neighborhood_name = NULL,
                                    input_community_district = NULL, convert_to="neighborhood") {

  if (sum(is.null(input_census_tract), is.null(input_neighborhood_name), is.null(input_community_district)) > 1) stop("Use only one input paramter!")

  if (is.null(input_census_tract) & is.null(input_neighborhood_name) & is.null(input_community_district)) stop("Use input parameter census_tract or neighborhood_name or community_district.")

  if (!is.null(input_census_tract)) {
    if (convert_to == "neighborhood") return(census_to_nbd(input_census_tract))
    if (convert_to == "community")      return(census_to_cmnty(input_census_tract))
    if (convert_to == "borough")      return(census_to_boro(input_census_tract))
    print("inside")
  } else if (!is.null(input_neighborhood_name)) {
    if (convert_to == "community")      return(nbd_to_cmnty(input_neighborhood_name))
    if (convert_to == "borough")      return(nbd_to_boro(input_neighborhood_name))
  } else {
    return(cmnty_to_boro(input_community_district))
  }

  }


census_to_nbd <- function(input_census_tract) {
  result <- census_tract_to_nbd_vec[input_census_tract]
  names(result) <- NULL
  result
}

census_to_boro <- function(input_census_tract) {
  result <- census_tract_to_boro_vec[input_census_tract]
  names(result) <- NULL
  result
  }

census_to_cmnty <- function(input_census_tract) {
  result <- census_tract_to_cmnty_vec[input_census_tract]
  names(result) <- NULL
  result
}

nbd_to_cmnty <- function(input_community_name) {
  result <- census_tract_to_cmnty_vec[input_census_tract]
  names(result) <- NULL
  result
}

nbd_to_boro <- function(input_neighborhood) {
  result <- nbd_to_boro_vec[input_neighborhood]
  names(result) <- NULL
  result
}

cmnty_to_boro <- function(input_community_name) {
  result <- cmnty_to_boro_vec[input_community_name]
  names(result) <- NULL
  result
}
