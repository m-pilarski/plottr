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
      height = unit_to_mm(purrr::chuck(.plot_grob, "heights", t)),
      width = unit_to_mm(purrr::chuck(.plot_grob, "widths", l)),
      margin_top = sum(vctrs::vec_slice(
        unit_to_mm(purrr::chuck(.plot_grob, "heights")), seq_len(t - 1)
      )),
      margin_left = sum(vctrs::vec_slice(
        unit_to_mm(purrr::chuck(.plot_grob, "widths")), seq_len(l - 1)
      )),
      margin_bottom = sum(vctrs::vec_slice(
        unit_to_mm(purrr::chuck(.plot_grob, "heights")), -seq_len(t)
      )),
      margin_right = sum(vctrs::vec_slice(
        unit_to_mm(purrr::chuck(.plot_grob, "widths")), -seq_len(l)
      )),
      .keep="none"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      margin_y = margin_top + margin_bottom,
      margin_x = margin_left + margin_right,
      dplyr::across(-name, function(..x){ggplot2::unit(..x, units="mm")})
    )

  return(.element_margin_data)

}
