
collapse_graph <-
  function(graph,
           graph_type = "censustract",
           sex_only = FALSE,
           map_within_neighborhood = NULL,
           map_within_community = NULL,
           map_within_borough = NULL) {

    nodes <- graph |>
      tidygraph::activate(nodes) |>
      data.frame()

    edges <- graph |>
      tidygraph::activate(edges) |>
      dplyr::mutate(
        from_censustract    = tidygraph::.N()$censustract[from],
        to_censustract      = tidygraph::.N()$censustract[to],
        from_neighborhood   = tidygraph::.N()$neighborhood[from],
        to_neighborhood     = tidygraph::.N()$neighborhood[to],
        from_community      = tidygraph::.N()$community[from],
        to_community        = tidygraph::.N()$community[to],
        from_borough        = tidygraph::.N()$borough[from],
        to_borough          = tidygraph::.N()$borough[to]
      ) |>
      data.frame()

    if (sex_only) edges <- edges |>
      dplyr::filter(sex == "yes")


    if (graph_type == "censustract") {

      new_nodes <- nodes |>
        dplyr::group_by(censustract) |>
        dplyr::summarize(
          censustract = dplyr::first(censustract),
          neighborhood = dplyr::first(neighborhood),
          community = dplyr::first(community),
          borough = dplyr::first(borough)
        ) |>
        dplyr::mutate(name = censustract)

      new_edges <- edges |>
        dplyr::mutate(
          from = ifelse(
            from_censustract < to_censustract,
            from_censustract,
            to_censustract
          ),
          to   = ifelse(
            from_censustract < to_censustract,
            to_censustract,
            from_censustract
          )
        ) |>
        dplyr::group_by(from, to) |>
        dplyr::summarize(
          from_neighborhood = dplyr::first(from_neighborhood),
          to_neighborhood   = dplyr::first(to_neighborhood),
          from_community    = dplyr::first(from_community),
          to_community      = dplyr::first(to_community),
          from_borough      = dplyr::first(from_borough),
          to_borough        = dplyr::first(to_borough),
          weight            = sum(weight)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          within_neighborhood = from_neighborhood == to_neighborhood,
          within_community    = from_community == to_community,
          within_borough      = from_borough == to_borough
        )

    }


    if (graph_type == "neighborhood") {
      new_nodes <- nodes |>
        dplyr::group_by(neighborhood) |>
        dplyr::summarize(community = dplyr::first(community),
                  borough = dplyr::first(borough)) |>
        dplyr::mutate(name = neighborhood)

      new_edges <- edges |>
        dplyr::mutate(
          from = ifelse(
            from_neighborhood < to_neighborhood,
            from_neighborhood,
            to_neighborhood
          ),
          to   = ifelse(
            from_neighborhood < to_neighborhood,
            to_neighborhood,
            from_neighborhood
          )
        ) |>
        dplyr::group_by(from, to) |>
        dplyr::summarize(
          from_community    = dplyr::first(from_community),
          to_community      = dplyr::first(to_community),
          from_borough      = dplyr::first(from_borough),
          to_borough        = dplyr::first(to_borough),
          weight            = sum(weight)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          within_community    = from_community == to_community,
          within_borough      = from_borough == to_borough
        )

    }


    if (graph_type == "community") {
      new_nodes <- nodes |>
        dplyr::group_by(community) |>
        dplyr::summarize(borough = dplyr::first(borough)) |>
        dplyr::mutate(name = community)

      new_edges <- edges |>
        dplyr::mutate(
          from = ifelse(
            from_community < to_community,
            from_community,
            to_community
          ),
          to   = ifelse(
            from_community < to_community,
            to_community,
            from_community
          )
        ) |>
        dplyr::group_by(from, to) |>
        dplyr::summarize(
          from_borough      = dplyr::first(from_borough),
          to_borough        = dplyr::first(to_borough),
          weight            = sum(weight)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          within_borough      = from_borough == to_borough
        )

    }


    if (graph_type == "borough") {
      new_nodes <- nodes |>
        dplyr::group_by(borough) |>
        dplyr::summarize() |>
        dplyr::mutate(name = borough)

      new_edges <- edges |>
        dplyr::mutate(
          from = ifelse(from_borough < to_borough,
                        from_borough,
                        to_borough),
          to   = ifelse(from_borough < to_borough,
                        to_borough,
                        from_borough)
        ) |>
        dplyr::group_by(from, to) |>
        dplyr::summarize(weight            = sum(weight))

    }


    result_graph <- tidygraph::tbl_graph(nodes = new_nodes, edges = new_edges) |>
      tidygraph::activate(edges) |>
      dplyr::filter(from != to)


    if (!is.null(map_within_neighborhood) & graph_type == "censustract") {
      result_graph <- result_graph |>
        tidygraph::activate(edges) |>
        dplyr::filter(within_neighborhood == map_within_neighborhood)
    }

    if (!is.null(map_within_community) & graph_type %in% c("censustract", "neighborhood")) {
      result_graph <- result_graph |>
        tidygraph::activate(edges) |>
        dplyr::filter(within_community == map_within_community)
    }

    if (!is.null(map_within_borough) & graph_type %in% c("censustract", "neighborhood", "community")) {
      result_graph <- result_graph |>
        dplyr::activate(edges) |>
        dplyr::filter(within_borough == map_within_borough)
    }


    result_graph |>
      tidygraph::activate(nodes) |>
      {function(x) dplyr::mutate(degree = igraph::strength(x))}()
  }
