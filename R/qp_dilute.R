#' Calculate dilutions from predicted concentrations
#'
#' @param x A `data.frame` or `list` containing a `data.frame` named `qp` with
#'   a column named `.pred_conc` or `.mean_pred_conc`. If both, will favor
#'   `.mean_pred_conc`.
#' @param target_conc Numeric vector. Target concentration in (mg/mL) protein.
#'   If length == 1, recycled.
#' @param target_vol Target volume in uL. If length == 1, recycled.
#'
#' @return Same as input, with the volumes of lysate and volumes of diluent to
#'   add.
#' @importFrom rlang .data
#' @export
qp_dilute <- function(x, ...) {
  UseMethod("qp_dilute")
}

#' @rdname qp_dilute
#' @export
qp_dilute.data.frame <- function(x, target_conc = NULL, target_vol = 15,
                                 remove_standards = FALSE, ...) {
  check_has_cols(x, c(".pred_conc", ".mean_pred_conc"), type = "or")

  if (remove_standards || is.null(target_conc)) {
    check_has_cols(x, "sample_type")
    check_sample_type(x$sample_type)
  }

  if (remove_standards)
    x <- x |> dplyr::filter(.data$sample_type != "standard")

  if (!length_is_recyclable(target_conc, x)) {
    rlang::abort("`target_conc` length is not 1 or nrow(x)")
  }

  conc_col_name <- ifelse(
    ".mean_pred_conc" %in% colnames(x), ".mean_pred_conc", ".pred_conc"
  )

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

  x <- x |>
    dplyr::bind_cols(dils) |>
    tidyr::unnest_wider(.data$value)
}

#' @rdname qp_dilute
#' @export
qp_dilute.list <- function(x, target_conc = NULL, target_vol = 15,
                           remove_standards = FALSE, ...) {
  x$qp <- qp_dilute(x$qp, target_conc, target_vol, remove_standards, ...)
  x
}

length_is_recyclable <- function(n, x) {
  !is.null(n) && (n == 1 || n == nrow(x))
}
