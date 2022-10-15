census_tract_sf_obj <- tigris::tracts(cb = TRUE, year=2020)

census_tract_centroids <- census_tract_sf_obj |>
  sf::st_centroid() |>
  dplyr::select(GEOID)  |>
  (function(.) dplyr::mutate(., coord = sf::st_coordinates(.)))() |>
  data.frame() |>
  (function(.) dplyr::mutate(., lon=coord[,1], lat=coord[,2]))() |>
  (function(.) dplyr::select(., -c(geometry, coord)))()

census_tract_neighbors <- spdep::poly2nb(census_tract_sf_obj)
names(census_tract_neighbors) <- census_tract_sf_obj$GEOID
census_tract_neighbors <- sapply(census_tract_neighbors, function(x) census_tract_sf_obj[x,]$GEOID, USE.NAMES = F)

neighborhood_names <- readr::read_csv("/Users/keletsomakofane/Documents/_gitrepos/mpxnyc/data-raw/nyc2020census_tract_nta_cdta_relationships.csv") |>
  dplyr::mutate(GEOID = as.character(GEOID))


boros <- sf::st_read("/Users/keletsomakofane/Documents/_gitrepos/mpxnyc/data-raw/nybb_22b/nybb.shp")


int_data <- list(
  census_tract_centroids = census_tract_centroids,
  census_tract_sf_obj = census_tract_sf_obj,
  census_tract_neighbors = census_tract_neighbors,
  neighborhood_names = neighborhood_names,
  boros = boros
)

usethis::use_data(int_data, int_data, internal=TRUE, overwrite=TRUE)


