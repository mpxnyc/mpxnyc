#' Convert Spatial Units in New York City
#'
#' @description Convert to parent spatial unit in hierarchy: borough > neighborhood > census_tract
#'
#' @param census_tract Vector (character) of input census tracts to convert to neighborhoods or boroughs
#' @param neighborhood_name Vector (character) of input neighborhoods to convert to boroughs
#' @param convert_to String showing how to convert census_tracts. Options are "neighborhood" (default) and "borough"
#'
#' @return Vector (character) of neighborhood or borrough names
#' @export
#'
#' @examples
convert_spatial_unit_ny <- function(census_tract = NULL, neighborhood_name = NULL, convert_to="neighborhood") {

  if (!is.null(census_tract) & !is.null(neighborhood_name)) stop("Use input parameter census_tract or neighborhood_name, not both!")

  if (is.null(census_tract) & is.null(neighborhood_name)) stop("Use input parameter census_tract or neighborhood_name.")


  if (!is.null(census_tract)) {
    if (convert_to == "neighborhood") return(census_to_nbd(census_tract))
    if (convert_to == "borough") return(census_to_boro(census_tract))
    print("inside")
  } else {
    return(nbd_to_boro_individual(neighborhood_name))
  }

  }


census_to_nbd_individual <- function(census_tract) {
  neighborhood_names %>%
    filter(GEOID %in% census_tract) %>%
    pull(NTAName) %>%
    {.[1]}
}

census_to_nbd <- function(census_tract) {
  purrr::map_chr(census_tract, census_to_nbd_individual)
}

census_to_boro_individual <- function(census_tract) {
  neighborhood_names %>%
    filter(GEOID %in% census_tract) %>%
    pull(BoroName)  %>%
    {.[1]}
}

census_to_boro <- function(census_tract) {
  purrr::map_chr(census_tract, census_to_boro_individual)
}

nbd_to_boro_individual <- function(nbd) {
  neighborhood_names %>%
    filter(NTAName == nbd) %>%
    pull(BoroName) %>%
    {.[1]}
}

nbd_to_boro <- function(nbd) {
  purrr::map_chr(nbd, nbd_to_boro_individual)
}
