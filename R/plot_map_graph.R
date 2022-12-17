#' Plot Network Graph
#'
#' @param graph_type Spatial Partitioning. Default "community". Other options are "censustract", "neighborhood", and "borough
#' @param weight_threshold Display parameter (numeric). Default 1.
#' @param degree_threshold Display parameter (numeric). Default 10.
#' @param sex_only Restrict to places where participant had sexual contact
#' @param map_within_neighborhood Restrict to within-neighborhood connections. Default NULL. Logical.
#' @param map_within_community Restrict to within-community connections. Default NULL. Logical.
#' @param map_within_borough Restrict to within-borough connections. Default NULL. Logical.
#' @param mapping_basegraph Accepts ggplot2::aes() object
#' @param mapping_spatial_units Accepts ggplot2::aes() object
#' @param mapping_lines Accepts ggplot2::aes() object
#' @param mapping_points Accepts ggplot2::aes() object
#' @param options_basegraph Accepts geom_sf options
#' @param options_spatial_units Accepts geom_sf options
#' @param options_lines Accepts geom_sf options
#' @param options_points Accepts geom_sf options
#' @param places_data Place dataset
#'
#' @return ggplot object
#' @export
#'

plot_map_graph <-
  function(graph_type = "community",
           weight_threshold = 1,
           degree_threshold = 10,
           sex_only = FALSE,
           map_within_neighborhood = NULL,
           map_within_community = NULL,
           map_within_borough = NULL,

           mapping_basegraph = NULL,
           mapping_spatial_units = NULL,
           mapping_lines = ggplot2::aes(alpha = weight),
           mapping_points = ggplot2::aes(size = degree),
           options_basegraph = list(data = mpxnyc::community_sf_obj, alpha = 1, size = 0.1, color = "white", fill = "#009BE8"),
           options_spatial_units = list(alpha = 0.4, size = 0.1),
           options_lines = list(color = "#41BFFF", size = 0.1),
           options_points = list( color = "#41BFFF", size = 0.5),
           places_data
  ) {


    mapdata <- make_graph(
      graph_type = graph_type,
      weight_threshold = weight_threshold,
      degree_threshold = degree_threshold,
      sex_only = sex_only,
      map_within_neighborhood = map_within_neighborhood,
      map_within_community = map_within_community,
      map_within_borough = map_within_borough,
      places_data = places_data
    ) %>%
      create_network_map()

    mapdata_full <- make_graph(
      graph_type = graph_type,
      weight_threshold = weight_threshold,
      degree_threshold = 1,
      sex_only = sex_only,
      map_within_neighborhood = map_within_neighborhood,
      map_within_community = map_within_community,
      map_within_borough = map_within_borough,
      places_data = places_data
    ) %>%
      create_network_map()

    result <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")


    if (!is.null(mapping_basegraph) | !is.null(options_basegraph)){
      options_basegraph$mapping <- mapping_basegraph
      result <- result + do.call(ggplot2::geom_sf, options_basegraph)
    }

    if (!is.null(mapping_spatial_units) | !is.null(options_spatial_units)){
      options_spatial_units$mapping <- mapping_basegraph
      options_spatial_units$data <- mapdata$nodes
      result <- result + do.call(ggplot2::geom_sf, options_spatial_units)
    }


    if (!is.null(mapping_lines) | !is.null(options_lines)){

      options_lines$data <-  mapdata_full$edges
      result <- result + do.call(ggplot2::geom_sf, options_lines) + do.call(ggplot2::geom_sf, list(mapping = mapping_lines, data = mapdata$edges, color = "#5D207A"))
    }

    if (!is.null(mapping_points) | !is.null(options_points)){

      options_points$data <- mapdata_full$nodes %>% sf::st_centroid()
      result <- result + do.call(ggplot2::geom_sf, options_points) + do.call(ggplot2::geom_sf, list(mapping = mapping_points, data = mapdata$nodes %>% sf::st_centroid(), color = "#5D207A"))
    }



    result +
      ggplot2::labs(title = "MPX NYC Group Sex Connection Patterns", subtitle = paste0("Degree Threshold = ", degree_threshold, " || Map Type = '", graph_type, "'")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, family = "Helvetica"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, family = "Helvetica"))


  }
