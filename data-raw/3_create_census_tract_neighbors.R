

census_tract_neighbors <- spdep::poly2nb(census_tract_sf_obj)
names(census_tract_neighbors) <- census_tract_sf_obj$GEOID
census_tract_neighbors <- sapply(census_tract_neighbors, function(x) census_tract_sf_obj[x,]$GEOID, USE.NAMES = F)

usethis::use_data(census_tract_neighbors, census_tract_neighbors, internal=TRUE, overwrite=TRUE)
