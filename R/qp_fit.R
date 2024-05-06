#' Fit an lm using standards absorbances
#'
#' @param x A `data.frame` or `list` containing a `data.frame` under the name
#'   `qp`. See details.
#'
#' @details The supplied `data.frame` must have the following columns:
#'   - `sample_type`. Character. If not 'standard', assumed to be a sample
#'   - `.is_outlier`. Boolean. If TRUE, assumed to be outlier and removed from
#'     fitting. If FALSE or NA, used for fitting. If unsupplied, will create one
#'     with all values set to NA.
#'   - `.conc`. Numeric. Known concentration of standard.
#'   - `.log2_abs`. Numeric. The log2 of the absorbances
#'
#' @return A list containing:
#'   - `fit`, an `lm` object fit with the formula `.log2_conc ~ .log2_abs`, fit
#'   using non-outlier standards
#'   - `qp`, the input data
#' @importFrom rlang .data
#' @examples
#'   absorbances |>
#'     qp_add_std_conc() |>
#'     qp_fit()
#' @export
qp_fit <- function(x) {
  UseMethod("qp_fit")
}

#' @rdname qp_fit
#' @export
qp_fit.data.frame <- function(x) {

  if (!has_cols(x, ".log2_abs")) {
    rlang::inform("Did not find column `.log2_abs`, calculating.")
    check_has_cols(x, ".abs")
    check_abs(x$.abs)
    x$.log2_abs <- log2(x$.abs)
  }

  x <- provide_outliers_if_none(x, "all")

  check_has_cols(x, c("sample_type", ".conc"))
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
#' @export
qp_fit.list <- function(x) {
  x$fit <- qp_fit.data.frame(x$qp)
  x
}
