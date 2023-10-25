#' Fit an lm using standards absorbances
#'
#' @param x A data.frame. See details.
#'
#' @details The supplied dataframe must have the following columns:
#'   - `sample_type`. Character. If not 'standard', assumed to be a sample
#'   - `.is_outlier`. Boolean. If TRUE, assumed to be outlier and removed from
#'     fitting. If FALSE or NA, used for fitting.
#'   - `.conc`. Numeric. Known concentration of standard.
#'   - `.log2_abs`. Numeric. The log2 of the absorbances
#'   The function creates a new column `.log2_conc`, or log2(`.conc` + 0.5)
#'
#' @return An `lm` object fit with the formula `.log2_conc ~ .log2_abs`, fit
#'   using non-outlier standards
#' @export
#' @importFrom rlang .data
qp_fit <- function(x) {
  standards <- x |>
    dplyr::filter(
      .data$sample_type == "standard",
      f_or_na(.data$.is_outlier)
    )
  fit_data <- dplyr::mutate(
    standards,
    .log2_conc = log2(.data$.conc + 0.5)
  )
  stats::lm(.log2_conc ~ .log2_abs, data = fit_data)
}
