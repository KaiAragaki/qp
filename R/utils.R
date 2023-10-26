f_or_na <- function(x) {
  !x | is.na(x)
}

# Calculate Dilutions ----------------------------------------------------------
#' Calculate dilutions for an analyzed `qp` `list`
#'
#' @param x The output of `qp()`
#' @param target_conc Target concentration in (mg/mL) protein
#' @param target_vol Target volume in uL
#'
#' @return A list, where the `qp` item has volumes of lysate and volumes of H2O
#' to add.
#' @export
qp_calc_dil <- function(x, target_conc, target_vol) {
  x$qp <- x$qp |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .temp = list(
        bladdr::dilute(.data$.pred_conc, target_conc, target_vol, quiet = TRUE)
      )
    ) |>
    tidyr::unnest_wider(.data$.temp)
  x
}
