plot_graph <-
  function(graph_type = "censustract",
           weight_threshold = 1,
           degree_threshold = 0,
           sex_only = FALSE,
           map_within_neighborhood = NULL,
           map_within_community = NULL,
           map_within_borough = NULL,

           mapping_lines = ggplot2::aes(),
           mapping_points = ggplot2::aes(size = degree),
           options_lines = list(alpha = 0.2, size = 0.5),
           options_points = list(alpha = 0.8),
           places_data
  ) {

    graph <- make_graph(
      graph_type = graph_type,
      weight_threshold = weight_threshold,
      degree_threshold = degree_threshold,
      sex_only = sex_only,
      map_within_neighborhood = map_within_neighborhood,
      map_within_community = map_within_community,
      map_within_borough = map_within_borough,
      places_data = places_data
    )

    options_lines$mapping <- mapping_lines
    options_points$mapping <- mapping_points

    graph %>%
      ggraph::ggraph(layout = "igraph", algorithm = "fr") +
      do.call(ggraph::geom_edge_link, options_lines) +
      do.call(ggraph::geom_node_point, options_points) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
  }
