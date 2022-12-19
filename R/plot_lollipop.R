plot_lolipop <- function(data, variables) {


    data$variable <- data[[variables[var]]]
    variable_name <- variables[var]
    plot_title    <- sjlabelled::get_label(data)[variables[var]] |> stringr::str_wrap(width = 60)

    if (is.factor(data$variable) | is.logical(data$variable)) {

      temp <- data |>
        dplyr::filter(!is.na(variable)) |>
        dplyr::mutate(value = 1) |>
        dplyr::group_by(variable) |>
        dplyr::summarize(value = sum(value)) |>
        dplyr::ungroup() |>
        dplyr::mutate(perc = paste0(round(100* value / sum(value)), "%"))


      figure <- ggplot(temp, aes(x=variable, y=value)) +
        geom_linerange(aes(x = variable, ymin =0, ymax=value), stat="identity", color=bar_color, size = 5) +
        #labs(x=variable_name , title=plot_title) +
        geom_point(aes(x = variable, y = value), size = 15, color="#009BE8") +
        geom_text(aes(label = perc, x = variable, y = value), color = "white") +
        theme(
          panel.background = element_rect(fill = "white", color = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          text = element_text(color = "black", size=16),
          axis.title = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.background = element_rect(fill = "white", color = "white"),
          plot.margin = unit(c(1,1,1,1), units="cm"),
          panel.margin = unit(c(1,1,1,1), units="cm"),
          legend.margin = unit(c(1,1,1,1), units="cm"),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0, unit = "pt"), hjust=0.5)
        )

      return(figure)
    }




}
