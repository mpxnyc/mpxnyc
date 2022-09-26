county_codes <- c("005", "047", "061", "081", "085")

census_tract_centroids <- purrr::map_df(county_codes, ~ tigris::tracts(state ="NY", county=.x, cb = TRUE, year=2020)) |>
  sf::st_centroid() |>
  dplyr::select(GEOID)  |>
  (function(.) dplyr::mutate(., coord = sf::st_coordinates(.)))() |>
  data.frame() |>
  (function(.) dplyr::select(., -c(geometry)))()


  usethis::use_data(census_tract_centroids, census_tract_centroids, internal=TRUE)


