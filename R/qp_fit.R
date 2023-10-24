#' Fit an lm using standards absorbances
#'
#'
#'
#' @param x A data.frame. See details.
#'
#' @details The supplied dataframe must have the following columns:
#' - `sample_type`. Character. If not 'standard', assumed to be a sample
#' - `.is_outlier`. Boolean. If TRUE, assumed to be outlier and removed from
#' fitting. If FALSE or NA, used for fitting.
#' - `.conc`. Numeric. Known concentration of standard.
#' - `.log2_abs`. Numeric. The log2 of the absorbances of 562
#' The function creates a new column
#'
#'
#' @return An `lm`

qp_fit <- function(x) {
  standards <- x |>
    dplyr::filter(
      .data$sample_type == "standard",
      f_or_na(.data$.is_outlier)
    )
  fit_data <- dplyr::mutate(standards, .log2_conc = log2(.data$.conc + .5))
  stats::lm(.log2_conc ~ .log2_abs, data = fit_data)
}
