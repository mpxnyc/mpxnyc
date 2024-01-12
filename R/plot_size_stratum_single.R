plot_size_stratum_single <- function(dot_size) {
  result <- ggplot2::ggplot()


  result <- result +
                  ggplot2::geom_point(aes(x = 0, y = 0, size = dot_size)) +
                  ggplot2::theme_void()
  result
}

