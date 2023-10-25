f_or_na <- function(x) {
  !x | is.na(x)
}

# Calculate Dilutions ----------------------------------------------------------
#' Calculate dilutions for an analyzed `qp` `list`
#'
#' @param x The output of `qp()`
#' @param target_conc Target concentration in (mg/mL) protein
#' @param target_vol Target volume in uL
#'
#' @return A list, where the `qp` item has volumes of lysate and volumes of H2O
#' to add.
#' @export
qp_calc_dil <- function(x, target_conc, target_vol) {
  x$qp <- x$qp |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .temp = list(
        bladdr::dilute(.data$pred_conc, target_conc, target_vol, quiet = TRUE)
      )
    ) |>
    tidyr::unnest_wider(.data$.temp)
  x
}

# Visualization ----------------------------------------------------------------
#' View the absorbances of an analyzed `qp` as they were on the plate
#'
#' @param x The output of `qp()`
#'
#' @return a `ggplot`
#' @export
make_qp_plate_view <- function(x) {
  x$gp |>
    gplate::gp_plot(.data$value) +
    ggplot2::geom_point(ggplot2::aes(color = .data$value), size = 20) +
    ggplot2::geom_text(
               ggplot2::aes(label = round(.data$value, 2)),
               color = "black"
             ) +
    ggplot2::scale_color_gradient(low = "darkseagreen1", high = "mediumpurple3")
}

#' View an absorbance/concentration plot
#'
#' @param x The output of `qp()`
#'
#' @return a `ggplot`
#' @export
make_qp_standard_plot <- function(x) {

  plot_data <- x$qp |>
    dplyr::mutate(
      outlier = !f_or_na(.data$is_outlier),
      y = ifelse(
        .data$sample_type == "standard",
        log2(.data$conc + 0.5),
        log2(.data$.pred_conc_mean + 0.5)
      )
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$log_abs, y = .data$y,
      color = .data$sample_type,
      shape = .data$outlier
    )) +
    ggplot2::scale_color_viridis_d(
               option = "viridis", end = 0.8, direction = -1
             ) +
    ggplot2::geom_abline(intercept = x$fit$coefficients[1],
                slope = x$fit$coefficients[2],
                size = 2,
                alpha = 0.2) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::scale_shape_manual(values = c(16, 4)) +
    ggplot2::labs(x = "Log2(Absorbance)", y = "Log2(Concentration + 0.5)")
}
