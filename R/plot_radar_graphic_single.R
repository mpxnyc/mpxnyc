plot_radar_graphic_single <- function(data, variable_name = "age", stratum_input = "Black\nCisgender\nMen", list_labels) {

  variable_levels       <- names(list_labels[[variable_name]][["levels"]])
  variable_labels       <- list_labels[[variable_name]][["levels"]]

  stratum_levels        <- names(list_labels[["stratum"]][["levels"]])
  stratum_labels        <- list_labels[["stratum"]][["levels"]]

  data  %>%
    dplyr::filter(
      variable == variable_name,
      stratum == stratum_input
    ) %>%
    dplyr::mutate(
      variable = as.character(variable),
      stratum  = as.character(stratum),
      level    = as.character(level)
    ) %>%
    dplyr::mutate(
      level = factor(level, variable_levels, variable_labels),
      stratum = factor(stratum, stratum_levels, stratum_labels)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(aes(x = level, fill = level, y = proportion), width = 1, stat = "identity") +
    ggplot2::coord_polar() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
          panel.grid.major.x = ggplot2::element_line(size = 0.1)) +
    ggplot2::scale_fill_manual(values = c(
      mpxnyc_colors$dark_blue,
      mpxnyc_colors$dark_pink,
      mpxnyc_colors$dark_purple,
      mpxnyc_colors$light_blue,
      mpxnyc_colors$light_pink,
      mpxnyc_colors$light_purple
    )) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::ylim(-0.5, 1)
}

