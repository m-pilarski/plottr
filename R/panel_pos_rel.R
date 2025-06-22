#' Title
#'
#' @param .x ...
#'
#' @returns integer()
#'
#' @examples NULL
unit_to_mm <- function(.x){
  grid::convertUnit(.x, "mm", valueOnly=TRUE)
}

#' Title
#'
#' @param .ggplot ...
#'
#' @returns tibble()
#' @export
#'
#' @examples NULL
calc_element_margin_data <- function(.plot_obj){

  .plot_grob <- ggplot2::ggplotGrob(.plot_obj)

  .element_margin_data <-
    .plot_grob |>
    purrr::chuck("layout") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      name = name,
      margin_top = sum(
        head(unit_to_mm(purrr::chuck(.plot_grob, "heights")), t - 1)
      ),
      margin_left = sum(
        head(unit_to_mm(purrr::chuck(.plot_grob, "widths")), l - 1)
      ),
      margin_bottom = sum(
        tail(unit_to_mm(purrr::chuck(.plot_grob, "heights")), -t)
      ),
      margin_right = sum(
        tail(unit_to_mm(purrr::chuck(.plot_grob, "widths")), -l)
      ),
      .keep="none"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(-name, \(..x){ggplot2::unit(..x, "mm")}),
      margin_y = margin_top + margin_bottom,
      margin_x = margin_left + margin_right,
    )

  return(.element_margin_data)

}
