# Calculate Dilutions ----------------------------------------------------------
#' Calculate dilutions for an analyzed `qp` `list`
#'
#' @param x A `data.frame` or `list` containing a `data.frame` named `qp` with
#'   a column named `.pred_conc` or `.mean_pred_conc`. Will favor
#'   `.mean_pred_conc`.
#' @param target_conc Numeric vector. Target concentration in (mg/mL) protein.
#'   If length == 1, recycled.
#' @param target_vol Target volume in uL. If length == 1, recycled.
#'
#' @return Same as input, with the volumes of lysate and volumes of H2O to add.
#' @export
qp_dilute <- function(x, ...) {
  UseMethod("qp_dilute")
}

#' @rdname qp_dilute
#' @export
qp_dilute.data.frame <- function(x, target_conc = NULL, target_vol = 15, ...) {
  check_has_cols(x, c(".pred_conc", ".mean_pred_conc"), type = "or")
  conc_col_name <- ifelse(".mean_pred_conc" %in% colnames(x),
                          ".mean_pred_conc", ".pred_conc")
  if (is.null(target_conc)) {
    rlang::inform("`target_conc` is missing, using lowest sample concentration")
    samples <- dplyr::filter(x, .data$sample_type == "unknown")
    target_conc <- min(samples[[conc_col_name]], na.rm = TRUE)
  }

  dils <- mapply(
    bladdr::dilute, x[[conc_col_name]], target_conc, target_vol,
    MoreArgs = list(quiet = TRUE),
    SIMPLIFY = FALSE
  ) |>
    tibble::enframe(name = NULL)

  x |>
    dplyr::bind_cols(dils) |>
    tidyr::unnest_wider(.data$value)
}

#' @rdname qp_dilute
#' @export
qp_dilute.list <- function(x, target_conc = NULL, target_vol = 15, ...) {
  x$qp <- qp_dilute(x$qp, target_conc, target_vol, ...)
  x
}
