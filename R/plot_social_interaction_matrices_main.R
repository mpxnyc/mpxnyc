#' Plot Data Matrices
#'
#' @param data data
#' @param input_variable variable
#' @param activity_setting_input c("home", "social", "sexual")
#' @param list_labels labels
#'
#' @return ggplot object
#'
plot_social_interaction_matrices_main <- function(data_contact_matrices, input_variable, activity_setting_input, list_labels){

  plot_data <- data_contact_matrices %>%
                dplyr::filter(variable == input_variable, activity_setting == activity_setting_input) %>%
                dplyr::transmute(
                                from_var            =  from,
                                to_var              = to,
                                risk_deviation,
                                variable,
                                activity_setting,
                                significant          = risk_deviation_ci_lb*risk_deviation_ci_ub > 0,
                                risk_deviation_label = scales::percent(risk_deviation, 1),
                                confint              = paste0(scales::percent(risk_deviation_ci_lb, 1, suffix = ""), " to ", scales::percent(risk_deviation_ci_ub, 1, suffix = ""))
                              ) %>%
                dplyr::mutate(
                  from_var = factor(as.character(from_var), names(list_labels) %>% rev(), list_labels %>% rev()),
                  to_var   = factor(as.character(to_var),   names(list_labels) ,          list_labels )
                )


  ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var), color = "white", size = 30, data = plot_data ) +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var),  size = 20.5, data = plot_data  %>% dplyr::filter(significant), color = "black", shape = "square" ) +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var),  size = 20.5, data = plot_data  %>% dplyr::filter(!significant), color = "#f0f0f0", shape = "square" ) +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var, color = risk_deviation),  size = 20, data = plot_data  %>% dplyr::filter(significant), shape = "square" ) +
    ggplot2::geom_point(ggplot2::aes(x = to_var, y = from_var),  size = 20, data = plot_data  %>% dplyr::filter(!significant), shape = "square" , color = "white") +
    ggplot2::geom_text(ggplot2::aes(x = to_var, y = from_var, label = confint, fill = risk_deviation),  size = 3, color = "black" ,  data = plot_data %>% dplyr::filter(significant), position = ggplot2::position_nudge(0,-0.2)) +
    ggplot2::geom_text(ggplot2::aes(x= to_var, y = from_var, label = risk_deviation_label), data = plot_data %>% dplyr::filter(significant), position = ggplot2::position_nudge(0, +0.1)) +
    ggplot2::geom_text(ggplot2::aes(x= to_var, y = from_var, label = risk_deviation_label), data = plot_data %>% dplyr::filter(!significant), size = 3) +
    ggplot2::scale_color_gradient2("Preference", midpoint = 0, low = mpxnyc_colors$dark_blue, high = mpxnyc_colors$dark_pink, na.value = "white") +
    ggplot2::scale_fill_gradient2("Preference", midpoint = 0, low = mpxnyc_colors$dark_blue, high = mpxnyc_colors$dark_pink, na.value = "white") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(5,5,5,5, "mm"),
      axis.text = ggplot2::element_text(angle = 45, size = 9, color ="#636363")
    )

}

