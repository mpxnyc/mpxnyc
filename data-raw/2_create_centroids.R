

census_tract_centroids <- census_tract_sf_obj |>
  sf::st_centroid() |>
  dplyr::select(GEOID)  |>
  (function(.) dplyr::mutate(., coord = sf::st_coordinates(.)))() |>
  data.frame() |>
  (function(.) dplyr::mutate(., lon=coord[,1], lat=coord[,2]))() |>
  (function(.) dplyr::select(., -c(geometry, coord)))()

  usethis::use_data(census_tract_centroids, census_tract_centroids, internal=TRUE, overwrite=TRUE)


