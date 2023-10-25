#' View an absorbance/concentration plot
#'
#' @param x The output of `qp()`
#'
#' @return a `ggplot`
#' @export
#' @importFrom rlang .data
#' @examples
#' absorbances |>
#'   qp() |>
#'   qp_plot_standards()
qp_plot_standards <- function(x) {

  plot_data <- x$qp |>
    dplyr::mutate(
      outlier = !f_or_na(.data$.is_outlier),
      y = ifelse(
        .data$sample_type == "standard",
        log2(.data$.conc + 0.5),
        log2(.data$.pred_conc_mean + 0.5)
      )
    )

  ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = .data$.log2_abs, y = .data$y,
      color = .data$sample_type,
      shape = .data$outlier
    )
  ) +
    ggplot2::scale_color_viridis_d(
      option = "viridis", end = 0.8, direction = -1
    ) +
    ggplot2::geom_abline(
      intercept = x$fit$coefficients[1],
      slope = x$fit$coefficients[2],
      size = 2,
      alpha = 0.2
    ) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::scale_shape_manual(values = c(16, 4)) +
    ggplot2::labs(x = "Log2(Absorbance)", y = "Log2(Concentration + 0.5)")
}
