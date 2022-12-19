make_graph <-
  function(graph_type = "censustract",
           weight_threshold = 1,
           degree_threshold = 0,
           type = NULL,
           sex_only = FALSE,
           map_within_neighborhood = NULL,
           map_within_community = NULL,
           map_within_borough = NULL,
           places_data) {

    edges <- places_data |>
      dplyr::transmute(
        from = censusTractHome,
        to = censusTractPlace,
        type = placeType,
        sex = placeSex,
        weight = 1
      )

    tidygraph::tbl_graph(edges = edges) |>
      dplyr::mutate(
        censustract = name,
        neighborhood = mpxnyc::convert_spatial_unit_ny(name, convert_to = "neighborhood"),
        community    = mpxnyc::convert_spatial_unit_ny(name, convert_to = "community"),
        borough    = mpxnyc::convert_spatial_unit_ny(name, convert_to = "borough"),
      ) |>
      dplyr::filter(!is.na(community)) |>
      collapse_graph(graph_type, sex_only, map_within_neighborhood = map_within_neighborhood,
                     map_within_community = map_within_community,
                     map_within_borough = map_within_borough) |>
      tidygraph::activate(edges) |>
      dplyr::filter(weight >= weight_threshold) |>
      tidygraph::activate(nodes)  |>
      dplyr::left_join(shape_list[[graph_type]], by = c("name" = "identifier")) |>
      dplyr::filter(degree >= degree_threshold) |>
      dplyr::mutate(component = igraph::components(.)$membership[name]) |>
      dplyr::mutate(componentSize =  igraph::components(.)$csize[component]) |>
      dplyr::mutate(componentName = ifelse(
        componentSize > 1,
        paste("Component", component),
        paste("Component Others")
      )) |>
      tidygraph::arrange(name) |>
      tidygraph::activate(edges) |>
      dplyr::mutate(componentName = tidygraph::.N()$componentName[from])


  }
