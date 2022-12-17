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
names(cmnty_to_boro_vec)        <- cmnty_to_boro_data$community


borough_sf_obj <- sf::st_read("data-raw/nybb_22b/nybb.shp") |>
  dplyr::select(BoroName, geometry) |>
  dplyr::rename(borough = BoroName)

borough_sf_obj <- sf::st_simplify(borough_sf_obj,
                                  preserveTopology = T,
                                  dTolerance = 200)

neighborhood_sf_obj  <- sf::st_read("data-raw/NYC_NTA_shp") |>
  dplyr::select(ntaname) |>
  dplyr::rename(neighborhood = ntaname)

neighborhood_sf_obj <- sf::st_simplify(neighborhood_sf_obj,
                                       preserveTopology = T,
                                       dTolerance = 200)

community_sf_obj  <- sf::st_read("data-raw/nycdta2020_22b/") |>
  dplyr::select(CDTA2020) |>
  dplyr::rename(community = CDTA2020)

community_sf_obj <- sf::st_simplify(community_sf_obj,
                                    preserveTopology = T,
                                    dTolerance = 200)


shape_list <- list()

shape_list$censustract <- mpxnyc::census_tract_sf_obj |>
  dplyr::transmute(identifier = census_tract)

shape_list$neighborhood <- mpxnyc::neighborhood_sf_obj|>
  dplyr::transmute(identifier = neighborhood)

shape_list$community <- mpxnyc::community_sf_obj |>
  dplyr::transmute(identifier = community)

shape_list$borough <- mpxnyc::borough_sf_obj |>
  dplyr::transmute(identifier = borough)

shape_list <-
  lapply(shape_list, function(x)
    sf::st_transform(x = x, crs = sf::st_crs(census_tract_sf_obj)))

usethis::use_data(census_tract_sf_obj, census_tract_sf_obj,                                                       internal=FALSE, overwrite=TRUE)
usethis::use_data(neighborhood_sf_obj, neighborhood_sf_obj,                                                       internal=FALSE, overwrite=TRUE)
usethis::use_data(borough_sf_obj, borough_sf_obj,                                                                 internal=FALSE, overwrite=TRUE)
usethis::use_data(census_tract_centroids, census_tract_to_nbd_vec, census_tract_to_boro_vec, census_tract_to_cmnty_vec,
                  nbd_to_cmnty_vec, nbd_to_boro_vec, cmnty_to_boro_vec, shape_list,                               internal=TRUE,  overwrite=TRUE)



