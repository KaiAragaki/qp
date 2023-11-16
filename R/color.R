#' The default color palette for qp
#'
#' It attempts to match the real life colors of a protein quantification
#' experiment, in combination with `abs_to_col`
qp_pal <- grDevices::colorRampPalette(
  c("#a8ff9f", "#9e67ab", "#975ab6", "#3D1452"),
  bias = 0.5
)(100)

#' Convert an absorbance to a hexidecimal color
#'
#' Takes an absorbance and converts it to a hexidecimal color. For the default
#' qp_pal palette, this should provide a color that approximates real life
#' color at the given absorbance.
#'
#' @details The absorbances have typical baseline absorbance (~ 0.07) removed,
#'   and then an index is calculated with a logistic curve of maximum 100 and a
#'   center of 0.15.
#'
#' @param abs Numeric. Absorbances.
#' @param pal Character. A vector of hexidecimal colors.
#' @return Character. Hexidecimal colors corresponding to absorbances.
abs_to_col <- function(abs, pal) {
  abs <- abs - 0.07
  scaled <- 100 / (1 + exp(-10 * (abs - 0.15)))
  idx <- round(scaled)
  pal[idx]
}
