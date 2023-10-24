# Fit conc ~ abs using standards absorbances -----------------------------------
qp_fit <- function(x) {
  standards <- x |>
    dplyr::filter(
      .data$sample_type == "standard",
      f_or_na(.data$is_outlier)
    )
  fit_data <- dplyr::mutate(standards, log_conc = log2(.data$conc + .5))
  stats::lm(log_conc ~ log_abs, data = fit_data)
}
