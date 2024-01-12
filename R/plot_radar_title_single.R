plot_radar_title_single <- function(title, showdot = FALSE) {
  result <- ggplot2::ggplot()

  if (showdot) {
    result <- result +
      ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), size = 30, color = "#f7fbff")
  }

  result <- result +
    ggplot2::geom_text(aes(x = 0, y = 0, label = title), size = 5) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -0.5, hjust = 0.5))

  result
}

