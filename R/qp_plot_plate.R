#' View the absorbances of an analyzed `qp` as they were on the plate
#'
#' @param x A `data.frame` with `.row`, `.col`, and `.abs` columns
#' @param size The size of the points used to illustrate the wells.
#'   Passed to geom_point.
#'
#' @return a `ggplot`
#' @export
#' @importFrom rlang .data
#' @examples
#' qp_plot_plate(absorbances)
qp_plot_plate <- function(x, size = 15) {
  ggplot2::ggplot(x, ggplot2::aes(
    x = .data$.col,
    y = .data$.row,
    color = abs_to_col(.data$.abs, qp_pal)
  )) +
    ggplot2::geom_point(size = size) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(.data$.abs, 2),
        color = ifelse(.data$.abs < 0.35, "black", "white")
      ),
      size = size / 3
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_color_identity()
}
