#' Remove empty wells from data
#'
#' @param x A `data.frame` containing columns `.pred_conc` and `sample_type`.
#'   See details.
#'
#' @details This function keeps any columns with positive `.pred_conc` or
#'   `sample_type == "standard"`
#'
#' @return Same as input
#' @export
#' @importFrom rlang .data
qp_remove_empty <- function(x) {
  dplyr::filter(
    x, .data$.pred_conc > 0 | .data$sample_type == "standard"
  )
}
