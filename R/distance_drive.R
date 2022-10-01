#' Drive Distance
#'
#' @description Returns the shortest driving distance between a pair of census tracts
#'
#' @param census_tract_from Origin census tract
#' @param census_tract_to Destination census tract
#'
#' @return vector of distances
#'

distance.drive <- function(census_tract_from, census_tract_to, key="")  {

    if(key=="")
      key = getPass::getPass(msg = "Google API Key:")

    centroids <- census_tract_centroids


    if(!(census_tract_from %in% centroids$GEOID) | !(census_tract_to %in% centroids$GEOID))
      stop("GEOIDs provided not in centroid file")

    centroids <- centroids |> dplyr::select(GEOID, lon, lat)

    api_res <- mapsapi::mp_directions(
      origin = as.matrix(centroids |> dplyr::filter(GEOID == census_tract_from) |> dplyr::select(lon, lat)),
      destination  = as.matrix(centroids |> dplyr::filter(GEOID == census_tract_to) |> dplyr::select(lon, lat)),
      mode = "driving",
      departure_time = lubridate::ymd_hms("2023/10/25 19:00:00"),
      key = key
    )
    dist <- xml2::xml_double(xml2::xml_find_all(api_res, "//route/leg/distance/value"))
    dur <- xml2::xml_double(xml2::xml_find_all(api_res, "//route/leg/duration/value"))
    res <- c(dist, dur)
    return(setNames(res, c("distance" , "duration")))

}
