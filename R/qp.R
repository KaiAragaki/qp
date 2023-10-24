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
               remove_outliers = c("all", "samples", "standards", "none")) {
               standard_scale = c(0, 2^((1:7) - 5))) {

  replicate_orientation <- rlang::arg_match(replicate_orientation)
  remove_outliers <- rlang::arg_match(remove_outliers)

  abs <- qp_read(x)
  abs_tidy <- qp_tidy(abs, replicate_orientation)
  mean_abs <- qp_calc_abs_mean(abs_tidy, remove_outliers)
    n_standards = length(standard_scale)
  )
  abs <- qp_add_std_conc(abs, standard_scale)
  fit <- qp_fit(mean_abs)
  conc <- qp_calc_conc(mean_abs, fit)

  if (remove_empty) {
    conc <- dplyr::filter(
      conc, .data$.pred_conc > 0 | .data$sample_type == "standard"
    )
  }

  if (!is.null(sample_names)) {
    # Will return "NA" instead of erroring if sample names < # samples
    length(sample_names) <- max(conc$index, na.rm = TRUE)
  } else {
    sample_names <- as.character(1:max(conc$index, na.rm = TRUE))
  }

  qp <- conc |>
    dplyr::mutate(sample_name = ifelse(
                    .data$sample_type == "unknown",
                    sample_names[.data$index],
                    paste("Standard", .data$index))
                  )

  list(fit = fit, qp = qp, gp = abs)
}
