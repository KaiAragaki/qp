#' Remove empty wells from data
#'
#' @param x A `data.frame` or `list` containing a `data.frame` named `qp`
#'   containing columns `.pred_conc` and `sample_type`. See details.
#'
#' @details This function keeps any columns with positive `.pred_conc` or
#'   `sample_type == "standard"`
#'
#'
#' @return Same as input
#' @importFrom rlang .data
#'
#' @examples
#'
#' df <- expand.grid(
#'   .pred_conc = 0:1,
#'   sample_type = c("standard", "unknown")
#' )
#'
#' df
#'
#' qp_remove_empty(df)
#'
#' @export
qp_remove_empty <- function(x) {
  UseMethod("qp_remove_empty")
}

#' @rdname qp_remove_empty
#' @export
qp_remove_empty.data.frame <- function(x) {
  dplyr::filter(x, .data$.pred_conc > 0 | .data$sample_type == "standard")
}

#' @rdname qp_remove_empty
#' @export
qp_remove_empty.list <- function(x) {
  x$qp <- qp_remove_empty(x$qp)
  x
}
