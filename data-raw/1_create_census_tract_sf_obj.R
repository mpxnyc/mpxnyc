census_tract_sf_obj <- tigris::tracts(cb = TRUE, year=2020)

usethis::use_data(census_tract_sf_obj, census_tract_sf_obj, internal=TRUE, overwrite=TRUE)


