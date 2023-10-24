#' Quantify protein concentration from a MicroBCA assay
#'
#' @param x A `spectramax`, `gp`, or `data.frame` object, or path to SPECTRAmax
#' .xls(x)/.txt file.
#' @param replicate_orientation Either 'h' or 'v' - see Details.
#' @param sample_names Character vector of sample names.
#' @param remove_empty Should wells that have less absorbance than the lowest
#' standard be dropped?
#'
#' @details The standards must be in ascending concentration starting in the
#'   upper left corner. Whether this is from from left to right or top to bottom
#'   can be specified in 'replicate orientation'. Note that 'replicate
#'   orientation' specified the direction that REPLICATES lie, NOT the direction
#'   the samples flow (which will be opposite).
#'
#' @return a `tibble`
#' @export
#' @importFrom rlang .data
qp <- function(x,
               replicate_orientation = c("h", "v"),
               sample_names = NULL,
               remove_empty = TRUE,
               ignore_outliers = c("all", "samples", "standards", "none"),
               standard_scale = c(0, 2^((1:7) - 5)),
               n_replicates = 3,
               wavelength = 562) {

  replicate_orientation <- rlang::arg_match(replicate_orientation)
  ignore_outliers <- rlang::arg_match(ignore_outliers)

  x <- qp_tidy(
    x,
    replicate_orientation,
    n_standards = length(standard_scale),
    n_replicates = n_replicates,
    wavelength = wavelength
  )
  x <- qp_add_std_conc(x, standard_scale)
  x <- qp_calc_abs_mean(x, ignore_outliers)
  x$.log2_abs <- log2(x$.abs)
  fit <- qp_fit(x)
  x <- qp_calc_conc(x, fit)

  if (remove_empty) {
    conc <- dplyr::filter(
      x, .data$.pred_conc > 0 | .data$sample_type == "standard"
    )
  }

  if (!is.null(sample_names)) {
    # Will return "NA" instead of erroring if sample names < # samples
    length(sample_names) <- max(x$index, na.rm = TRUE)
  } else {
    sample_names <- as.character(1:max(x$index, na.rm = TRUE))
  }

  qp <- x |>
    dplyr::mutate(
             .sample_name = ifelse(
               .data$sample_type == "unknown",
               .data$.sample_names[.data$index],
               paste("Standard", .data$index))
           )
  list(fit = fit, qp = qp, gp = abs)
}
