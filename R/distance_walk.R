#' Walking Distance
#'
#' @description Returns the shortest walking distance between a pair of census tracts
#'
#' @param census_tract_from Origin census tract
#' @param census_tract_to Destination census tract
#'
#' @return vector of distances
#' @export
#'
#' @examples
distance.walk <- function(census_tract_from, census_tract_to, key="")  {
    require(dplyr)
    require(mapsapi)
    require(getPass)
    require(xml2)
    if(!file.exists(tract_centroid_file))
      stop("Census tract centroid file not found. Run get_NYC_tracts() or point to correct path.")

    #Google API Key: AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc
    if(key=="")
      key = getPass(msg = "Google API Key:")

    centroids <- readRDS("NYC_CT_centroids.rds")
    if(F %in% (c("GEOID", "lon", "lat") %in% colnames(centroids)))
      stop("centroids file needs columns named 'GEOID', 'lon', 'lat'")

    if(!(CT_1_GEOID %in% centroids$GEOID) | !(CT_2_GEOID %in% centroids$GEOID))
      stop("GEOIDs provided not in centroid file")

    centroids <- centroids %>% select(GEOID, lon, lat)
    api_res <- mp_directions(
      origin = as.matrix(centroids %>% filter(GEOID == CT_1_GEOID) %>% select(lon, lat)),
      destination  = as.matrix(centroids %>% filter(GEOID == CT_2_GEOID) %>% select(lon, lat)),
      mode = "walking",
      key = key
    )
    dist <- xml_double(xml_find_all(api_res, "//route/leg/distance/value"))
    dur <- xml_double(xml_find_all(api_res, "//route/leg/duration/value"))
    res <- c(dist, dur)
    return(setNames(res, c("distance" , "duration")))

}
