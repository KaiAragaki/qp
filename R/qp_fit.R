#' Fit an lm using standards absorbances
#'
#' @param x A `data.frame` or `lsit` containing a `data.frame` under the name
#'   `qp`. See details.
#'
#' @details The supplied dataframe must have the following columns:
#'   - `sample_type`. Character. If not 'standard', assumed to be a sample
#'   - `.is_outlier`. Boolean. If TRUE, assumed to be outlier and removed from
#'     fitting. If FALSE or NA, used for fitting.
#'   - `.conc`. Numeric. Known concentration of standard.
#'   - `.log2_abs`. Numeric. The log2 of the absorbances
#'   The function creates a new column `.log2_conc`, or log2(`.conc` + 0.5)
#'
#' @return A list containing:
#'   - `fit`, an `lm` object fit with the formula `.log2_conc ~ .log2_abs`, fit
#'   using non-outlier standards
#'   - `qp`, the input data
#' @export
#' @importFrom rlang .data
qp_fit <- function(x) {
  UseMethod("qp_fit")
}

#' @rdname qp_fit
qp_fit.data.frame <- function(x) {
  check_has_cols(x, c("sample_type", ".is_outlier", ".conc", ".log2_abs"))
  check_sample_type(x$sample_type)
  check_is_outlier(x$.is_outlier)
  check_conc(x$.conc)
  check_log2_abs(x$.log2_abs)

  standards <- x |>
    dplyr::filter(
      .data$sample_type == "standard",
      f_or_na(.data$.is_outlier)
    )
  fit_data <- dplyr::mutate(
    standards,
    .log2_conc = log2(.data$.conc + 0.5)
  )
  fit <- stats::lm(.log2_conc ~ .log2_abs, data = fit_data)
  list(fit = fit, qp = x)
}

#' @rdname qp_fit
qp_fit.list <- function(x) {
  x$fit <- qp_fit.data.frame(x$qp)
  x
}