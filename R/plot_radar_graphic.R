

#' Plot Radar Graph
#'
#' @param data plot data
#' @param list_labels labels
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_radar_graphic <- function(data, list_labels) {

  main_plots_list        <- list_labels %>%
                                  names() %>%
                                  setdiff(c("stratum")) %>%
                                  purrr::map(
                                    function(variable_name){

                                      list_labels$stratum$levels %>%
                                        names() %>%
                                        purrr::map(
                                          function(stratum_input){
                                            plot_radar_graphic_single(data, variable_name = variable_name, stratum_input = stratum_input, list_labels = list_labels )
                                          }
                                        )  %>%
                                        setNames(list_labels$stratum$levels) %>%
                                        cowplot::plot_grid(
                                                              plot_radar_title_single(list_labels[[variable_name]][["label"]]),
                                                              plot_radar_legend_single(data, variable_name, list_labels),
                                                              plotlist = .,
                                                              ncol = 1,
                                                              rel_heights = c(1, 2, rep(1, length(.)))
                                        )
                                      }
                                    )



  labels_length          <-   data %>%
                                  dplyr::pull(stratum) %>%
                                  levels() %>%
                                  length()

  labels                 <-   data %>%
                                  dplyr::mutate(stratum = factor(stratum, names(list_labels[["stratum"]][["levels"]]), list_labels[["stratum"]][["levels"]])) %>%
                                  dplyr::pull(stratum) %>%
                                  levels() %>%
                                  purrr::map(
                                    function(level) {
                                      plot_radar_title_single(level)
                                    }) %>%
                                  cowplot::plot_grid(
                                    plot_radar_title_single(""),
                                    plot_radar_title_single(""),
                                    plotlist = .,
                                    ncol = 1,
                                    rel_heights = c(1, 2, rep(1, length(.)))
                                  )


  cowplot::plot_grid(labels, plotlist = main_plots_list, nrow = 1)

}

