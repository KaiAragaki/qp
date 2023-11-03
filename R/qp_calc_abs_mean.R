#' Calculate absorbance means with optional outlier removal
#'
#' @param x A `data.frame` or `list` containing a `data.frame` named `qp`.
#'   See details.
#' @param ignore_outliers Which sample types should have outliers ignored from
#'   their mean calculations? If `.is_outlier` column is supplied, this argument
#'   is ignored.
#'
#' @details Input `data.frame` must contain the following columns:
#' - `sample_type`. Character. Must contain values either "standard" or
#'   "unknown"
#' - `index`. Numeric. Denotes sample number.
#' - `.abs`. Numeric. Contains absorbance values.
#' - If a boolean `.is_outlier` is supplied, that will be used instead.
#'
#' @return The input `tibble` with an `.is_outlier` column and a `.mean` column
#'
#' @importFrom rlang .data
#' @export
qp_calc_abs_mean <- function(x,
                             ignore_outliers = c(
                               "all", "standards", "samples", "none"
                             )) {
  UseMethod("qp_calc_abs_mean")
}

#' @rdname qp_calc_abs_mean
#' @export
qp_calc_abs_mean.data.frame <- function(x,
                                        ignore_outliers = c(
                                          "all", "standards", "samples", "none"
                                        )) {
  ignore_outliers <- rlang::arg_match(ignore_outliers)
  check_has_cols(x, c("sample_type", "index", ".abs"))
  check_sample_type(x$sample_type)
  check_index(x$index)
  check_abs(x$.abs)

  if (has_cols(x, ".is_outlier")) {
    cli::cli_inform("Data has `.is_outlier` column, using that")
  } else {
    x <- qp_mark_outliers(x, ignore_outliers)
  }

  dplyr::mutate(
    x,
    .mean = mean(.data$.abs[!.data$.is_outlier], na.rm = TRUE),
    .by = c("sample_type", "index")
  )
}

#' @rdname qp_calc_abs_mean
#' @export
qp_calc_abs_mean.list <- function(x,
                                  ignore_outliers = c(
                                    "all", "standards", "samples", "none"
                                  )) {
  ignore_outliers <- rlang::arg_match(ignore_outliers)
  x$qp <- qp_calc_abs_mean(x$qp, ignore_outliers)
  x
}
