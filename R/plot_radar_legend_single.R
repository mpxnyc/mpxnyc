plot_radar_legend_single <- function(data, variable_name, list_labels) {

  variable_levels <- names(list_labels[[variable_name]][["levels"]])
  variable_labels <- list_labels[[variable_name]][["levels"]]

  data %>%
    dplyr::filter(variable == variable_name) %>%
    dplyr::mutate(count = 10) %>%
    dplyr::mutate(level = as.character(level)) %>%
    dplyr::mutate(level = factor(level, variable_levels, variable_labels)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(x = level, y = count, fill = level),
      width = 1,
      stat = "identity",
      alpha = 0.1
    ) +
    ggplot2::geom_text(aes(x = level, y = count * 2, label = level)) +
    ggplot2::coord_polar() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
          panel.grid.major.x = ggplot2::element_line(size = 0.1)) +
    ggplot2::scale_fill_manual(
      values = c(
        mpxnyc_colors$dark_blue,
        mpxnyc_colors$dark_pink,
        mpxnyc_colors$dark_purple,
        mpxnyc_colors$light_blue,
        mpxnyc_colors$light_pink,
        mpxnyc_colors$light_purple
      )
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::ylim(-50, 50)
}
