boros <- sf::st_read("/Users/keletsomakofane/Documents/_gitrepos/mpxnyc/data-raw/nybb_22b/nybb.shp")

usethis::use_data(boros, boros, internal=TRUE, overwrite=TRUE)
