#' Remove empty wells from data
#'
#' @param x A `data.frame` containing columns `.pred_conc` and `sample_type` or
#'   `list` containing data in `x$qp`. See details.
#'
#' @details This function keeps any columns with positive `.pred_conc` or
#'   `sample_type == "standard"`
#'
#' @return Same as input
#' @export
#' @importFrom rlang .data
qp_remove_empty <- function(x) {
  if (is.list(x)) {
    qp <- x$qp
  } else {
    qp <- x
  }

  qp <- dplyr::filter(
    qp, .data$.pred_conc > 0 | .data$sample_type == "standard"
  )

  if (is.list(x)) {
    x$qp <- qp
    return(x)
  } else {
    return(qp)
  }
}
