#' Plot Data Matrices Large
#'
#' @param data data
#' @param input_variable Variable
#' @param activity_setting_inputs c("home", "social", "sexual")
#' @param spacing spacing
#' @param list_labels labels
#'
#' @return ggplot Object
#' @export
#'
plot_data_matrices_large <- function(data, input_variable, activity_setting_inputs,  spacing = 5, list_labels){

  plot_data <- data  %>%
                  dplyr::filter(variable == input_variable, activity_setting %in% activity_setting_inputs) %>%
                  dplyr::transmute(
                                  from_var             = from,
                                  to_var               = to,
                                  risk_deviation,
                                  variable,
                                  activity_setting,
                                  significant          = risk_deviation_ci_lb*risk_deviation_ci_ub > 0,
                                  risk_deviation_label = scales::percent(risk_deviation, 1),
                                  confint              = paste0(scales::percent(risk_deviation_ci_lb, 1), " to ", scales::percent(risk_deviation_ci_ub, 1)),
                                  sign                 = risk_deviation > 0
                                ) %>%
                  dplyr::mutate(
                    from_var = factor(from_var, names(list_labels) %>% rev(), list_labels %>% rev()),
                    to_var  = factor(to_var, names(list_labels), list_labels )
                  )

  ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var), color = "#efedf5",  data = plot_data ) +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var, color = sign),  data = plot_data %>% dplyr::filter(significant)) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete( ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing   = ggplot2::unit(20, "mm")
    )  +
    ggplot2::facet_grid(.~activity_setting, switch = "x") +
    ggplot2::scale_color_manual(values = c(mpxnyc_colors$dark_blue, mpxnyc_colors$dark_pink))

}

