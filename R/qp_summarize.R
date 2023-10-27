#' Summarize output from qp pipeline
#'
#' @param x A `data.frame` or a `list` containing a `data.frame` named `qp`
#' @return A `tibble` with the sample `name`, `sample_type`, and the mean of
#'   its predicted concentration (`.mean_pred_conc`)
#' @importFrom rlang .data
#' @export
qp_summarize <- function(x) {
  UseMethod("qp_summarize")
}

#' @rdname qp_summarize
#' @export
qp_summarize.data.frame <- function(x) {
  # FIXME - if sample name does not exist, make it exist (use index)
  check_has_cols(x, c("index", "sample_type", ".pred_conc_mean"))
  check_sample_type(x$sample_type)
  check_pred_conc_mean(x$.pred_conc_mean)
  x |>
    dplyr::select(name = ".sample_name", "sample_type", ".pred_conc_mean") |>
    dplyr::summarize(
      .mean_pred_conc = mean(.data$.pred_conc_mean, na.rm = TRUE),
      .by = c("name", "sample_type")
    ) |>
    dplyr::arrange(.data$sample_type)
}

#' @rdname qp_summarize
#' @export
qp_summarize.list <- function(x) {
  qp_summarize(x$qp)
}
