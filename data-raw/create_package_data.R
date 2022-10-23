census_tract_sf_obj <- tigris::tracts(state="NY", cb = TRUE, year=2020) |>
  rbind(tigris::tracts(state="NJ", cb = TRUE, year=2020)) |>
  dplyr::select(GEOID, NAMELSADCO, STATE_NAME) |>
  dplyr::rename(census_tract = GEOID, county = NAMELSADCO, state = STATE_NAME)

census_tract_centroids <- census_tract_sf_obj |>
  sf::st_centroid() |>
  dplyr::select(census_tract)  |>
  (function(.) dplyr::mutate(., coord = sf::st_coordinates(.)))() |>
  data.frame() |>
  (function(.) dplyr::mutate(., lon=coord[,1], lat=coord[,2]))() |>
  (function(.) dplyr::select(., -c(geometry, coord)))()


census_tract_to_boro_nbd_data <- readr::read_csv("data-raw/nyc2020census_tract_nta_cdta_relationships.csv") |>
  dplyr::mutate(GEOID = as.character(GEOID)) |>
  dplyr::select(GEOID, BoroName, NTAName, CDTACode) |>
  dplyr::rename(census_tract = GEOID, borough = BoroName, neighborhood = NTAName, community = CDTACode) |>
  data.table::data.table() |>
  data.table::setkey(census_tract)

census_tract_to_nbd_vec         <- census_tract_to_boro_nbd_data$neighborhood
names(census_tract_to_nbd_vec)  <- census_tract_to_boro_nbd_data$census_tract

census_tract_to_cmnty_vec         <- census_tract_to_boro_nbd_data$community
names(census_tract_to_cmnty_vec)  <- census_tract_to_boro_nbd_data$census_tract

census_tract_to_boro_vec         <- census_tract_to_boro_nbd_data$borough
names(census_tract_to_boro_vec)  <- census_tract_to_boro_nbd_data$census_tract

nbd_to_cmnty_data <- census_tract_to_boro_nbd_data[,c("neighborhood", "community")] |>
  unique(by = "neighborhood")

nbd_to_cmnty_vec               <- nbd_to_cmnty_data$community
names(nbd_to_cmnty_vec)        <- nbd_to_cmnty_data$neighborhood

nbd_to_boro_data <- census_tract_to_boro_nbd_data[,c("neighborhood", "borough")] |>
  unique(by = "neighborhood")

nbd_to_boro_vec               <- nbd_to_boro_data$borough
names(nbd_to_boro_vec)        <- nbd_to_boro_data$neighborhood

cmnty_to_boro_data <- census_tract_to_boro_nbd_data[,c("community", "borough")] |>
  unique(by = "community")

cmnty_to_boro_vec               <- cmnty_to_boro_data$borough
names(cmnty_to_boro_vec)        <- cmnty_to_boro_data$neighborhood


borough_sf_obj <- sf::st_read("/data-raw/nybb_22b/nybb.shp") |>
  dplyr::select(BoroName, geometry) |>
  dplyr::rename(borough = BoroName)

neighborhood_sf_obj  <- sf::st_read("data-raw/NYC_NTA_shp") |>
  dplyr::select(ntaname) |>
  dplyr::rename(neighborhood = ntaname)

community_sf_obj  <- sf::st_read("data-raw/nycdta2020_22b/") |>
  dplyr::select(CDTA2020) |>
  dplyr::rename(community = CDTA2020)


usethis::use_data(census_tract_sf_obj, census_tract_sf_obj,                                           internal=FALSE, overwrite=TRUE)
usethis::use_data(neighborhood_sf_obj, neighborhood_sf_obj,                                           internal=FALSE, overwrite=TRUE)
usethis::use_data(borough_sf_obj, borough_sf_obj,                                                     internal=FALSE, overwrite=TRUE)
usethis::use_data(community_sf_obj, community_sf_obj,                                                 internal=FALSE, overwrite=TRUE)
usethis::use_data(census_tract_centroids, census_tract_to_nbd_vec,
                  census_tract_to_boro_vec, census_tract_to_cmnty_vec,
                  nbd_to_cmnty_vec, nbd_to_boro_vec, cmnty_to_boro_vec,                               internal=TRUE,  overwrite=TRUE)


