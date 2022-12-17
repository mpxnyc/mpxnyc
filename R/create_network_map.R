create_network_map <- function(graph) {

  graph <- graph %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(coord_from = tidygraph::.N()$geometry[from] %>% sf::st_centroid(),
           coord_to   = tidygraph::.N()$geometry[to] %>% sf::st_centroid(),
           componentName = tidygraph::.N()$componentName[from]
    ) %>%
    dplyr::filter(from != to)

  edges <- graph %>%
    data.frame() %>%
    dplyr::mutate(geometry = NA)

  for (i in seq_along(edges$coord_from)) {
    edges$geometry[i] <-
      sf::st_cast(sf::st_union(c(edges$coord_from[i], edges$coord_to[i])), "LINESTRING")
  }

  a <- edges %>%
    dplyr::select(-c(coord_from, coord_to)) %>%
    sf::st_as_sf()


  nodes <- graph %>%
    tidygraph::activate(nodes) %>%
    data.frame() %>%
    dplyr::mutate(pre_geometry = geometry, geometry = NA)

  for (i in seq_along(nodes$geometry)) {
    nodes$geometry[i] <- nodes$pre_geometry[i]
  }

  b <- nodes %>%
    dplyr::select(-pre_geometry) %>%
    sf::st_as_sf()

  sf::st_crs(a) <- sf::st_crs(census_tract_sf_obj)
  sf::st_crs(b) <- sf::st_crs(census_tract_sf_obj)

  list(nodes = b, edges = a)
}
