#' Plot Data Matrices Combined
#'
#' @param data data
#' @param input_variable which
#' @param activity_setting_input  which
#' @param activity_setting_inputs which
#' @param spacing 5
#' @param list_labels labels
#'
#' @return ggplot object
#' @export
#'
plot_data_combined <- function(data, input_variable, activity_setting_input, activity_setting_inputs, spacing = 5, list_labels){
  plot_1 <- plot_data_matrices(data, input_variable, activity_setting_input, list_labels)
  plot_2 <- plot_data_matrices_large(data, input_variable, activity_setting_inputs, spacing, list_labels)

  cowplot::plot_grid(plot_1, plot_2, ncol = 1, rel_heights = c(10,2))
}
