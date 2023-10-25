#' View the absorbances of an analyzed `qp` as they were on the plate
#'
#' @param x A `data.frame` with `.row`, `.col`, and `.abs` columns
#'
#' @return a `ggplot`
#' @export
#' @importFrom rlang .data
#' @examples
#' x <- expand.grid(.row = 1:8, .col = 1:12)
#' x$.abs <- 1:96
#' make_qp_plate_view(x)
qp_plot_plate <- function(x) {
  ggplot2::ggplot(x, ggplot2::aes(
    x = .data$.col,
    y = .data$.row,
    color = .data$.abs
  )) +
    ggplot2::geom_point(size = 20) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$.abs, 2)),
      color = "black"
    ) +
    ggplot2::scale_color_gradient(low = "darkseagreen1", high = "mediumpurple3")
}
