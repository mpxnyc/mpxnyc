#' Drive Distance
#'
#' @description Returns the shortest driving distance between a pair of census tracts
#'
#' @param census_tract_from Origin census tract
#' @param census_tract_to Destination census tract
#'
#' @return vector of distances
#'

distance.drive <- function(census_tract_from, census_tract_to, key="", quiet = T)  {

    if(key=="")
      key = getPass::getPass(msg = "Google API Key:")

    centroids <- census_tract_centroids


    if(!(census_tract_from %in% centroids$census_tract) | !(census_tract_to %in% centroids$census_tract)){
      warning("Location not within NYC, returning NA")
      dist <- NA
      dur <- NA
      res <- c(dist, dur)
      return(setNames(res, c("distance" , "duration")))
    }

    if(census_tract_from == census_tract_to){
      warning("Origin and destination are the same, returning 0s")
      dist <- 0
      dur <- 0
      res <- c(dist, dur)
      return(setNames(res, c("distance" , "duration")))
    }

    centroids <- centroids |> dplyr::select(census_tract, lon, lat)



    api_res <- mapsapi::mp_directions(
      origin = as.matrix(centroids |> dplyr::filter(census_tract == census_tract_from) |> dplyr::select(lon, lat)),
      destination  = as.matrix(centroids |> dplyr::filter(census_tract == census_tract_to) |> dplyr::select(lon, lat)),
      mode = "driving",
      departure_time = lubridate::ymd_hms("2023/10/25 19:00:00"),
      key = key,
      quiet = quiet
    )

    if(xml2::xml_text(xml2::xml_find_all(api_res, "//status")) != "OK"){
      warning("Failed to get directions, returning NA")
      dist <- NA
      dur <- NA
      res <- c(dist, dur)
      return(setNames(res, c("distance" , "duration")))
    }

    dist <- xml2::xml_double(xml2::xml_find_all(api_res, "//route/leg/distance/value"))
    dur <- xml2::xml_double(xml2::xml_find_all(api_res, "//route/leg/duration/value"))
    res <- c(dist, dur)
    return(setNames(res, c("distance" , "duration")))

}
