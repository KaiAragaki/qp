f_or_na <- function(x) {
  !x | is.na(x)
}

#' Round volume to be pipette-compatible
#'
#' @param x Numeric. Volume to be rounded
#'
#' @return Numeric. Rounded volume.
#'
#' @examples
#' make_pipette_vol(104.13398)
#' make_pipette_vol(15.3331)
#' make_pipette_vol(9.9211)
#' # Vectorized:
#' make_pipette_vol(c(104.13398, 15.3331, 9.9211, NA, -100.1))
make_pipette_vol <- function(x) {
  a <- abs(x)
  a <- dplyr::case_when(
    is.na(a) ~ a,
    a > 200 ~ round(a),
    a > 20 ~ round(a / 2, 1) * 2,
    a > 10 ~ round(a / 2, 2) * 2,
    .default = round(a, 2)
  )
  a * sign(x)
}

#' Calculate dilution from known concentrations
#'
#' @param c1 Numeric. Initial concentration of sample.
#' @param c2 Numeric. Target concentration of sample.
#' @param v2 Numeric. Target final volume of sample. If `round_for_pipettes =
#'   TRUE`, assumes volume is uL.
#' @param round_for_pipettes Logical. If TRUE, rounds values to the accuracy of
#'   standard pipettes using `make_pipette_vol`.
#' @param quiet Logical. If FALSE, will warn when dilution is impossible to do
#'   without concentrating sample.
#'
#' @return a data.frame, with `sample_to_add` as the volume of sample to add,
#'   and `add_to` as the volume to dilute the sample into.
#'
#' @examples
#' dilute(203, 70, 10)
#' dilute(203, 70, 10, round_for_pipettes = FALSE)
#' # Vectorized:
#' dilute(c(8, 10, 12), c(4, 5, 6), c(7, 8, 9))
dilute <- function(c1,
                   c2 = min(c1),
                   v2,
                   round_for_pipettes = TRUE) {
  v1 <- (c2 * v2) / c1
  x <- data.frame(sample_to_add = v1, add_to = v2 - v1)
  if (round_for_pipettes)
    x <- apply(x, 2, make_pipette_vol, simplify = FALSE)
  as.data.frame(x)
}
