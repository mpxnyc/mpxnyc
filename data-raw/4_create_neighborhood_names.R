neighborhood_names <- readr::read_csv("/Users/keletsomakofane/Documents/_gitrepos/mpxnyc/data-raw/nyc2020census_tract_nta_cdta_relationships.csv") %>%
  mutate(GEOID = as.character(GEOID))

usethis::use_data(neighborhood_names, neighborhood_names, internal=TRUE, overwrite=TRUE)
