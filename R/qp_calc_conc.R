#' Predict concentrations from standards fit
#' @param x A list. See details.
#' @param ignore_outliers Boolean. Should outliers be considered when
#'   calculating the mean? See details.
#' @param group_cols Character vector. Columns to group by before taking the
#'   mean.
#'
#' @details The supplied list should contain two items - `fit`, generated by
#'   `qp_fit`, and `qp`, a data.frame. `qp` should contain the following:
#'   - Columns used in `fit`. Usually, this is `.log2_abs`
#'   - Any columns in `group_cols`
#'   - If `ignore_outliers = TRUE`, `.is_outlier` will be used if supplied,
#'     or created if not.
#' @return Returns a `list` with the input fit and data.frame, with additional
#'   columns:
#'   - `.pred`: The predicted value from the provided model
#'   - `.pred_conc`: `.pred`, transformed by `conc_transform`
#'   - `.pred_conc_mean`: The mean of `.pred_conc`, sans samples where column
#'     `.is_outlier == TRUE`
#'
#' @examples
#'
#' data <- system.file("extdata", "absorbances.txt", package = "qp")
#' calculated <- qp(data, replicate_orientation = "h")
#'
#' # Making a minimal object:
#' calculated$qp <- calculated$qp |>
#'   dplyr::select(
#'     .log2_abs, sample_type, index, .is_outlier
#'   )
#'
#' calculated
#'
#' qp_calc_conc(calculated)
#'
#' @export
#' @importFrom rlang .data
qp_calc_conc <- function(x,
                         ignore_outliers = TRUE,
                         group_cols = c("sample_type", "index")) {
  if (is.data.frame(x)) {
    rlang::warn(c(
      "`x` is a `data.frame`, not a `list`",
      "Trying to apply `qp_fit` to `x`"
    ))
    x <- qp_fit(x)
  }

  group_to_ignore <- ifelse(ignore_outliers, "all", "none")

  qp <- x$qp
  fit <- x$fit
  check_has_cols(qp, c(group_cols, all.vars(stats::terms(fit))[-1]))

  with_predictions <- dplyr::bind_cols(qp, .pred = stats::predict(fit, qp))
  x <- with_predictions |>
    dplyr::mutate(.pred_conc = 2^(.data$.pred) - 0.5) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_cols))) |>
    provide_outliers_if_none(group_to_ignore) |>
    dplyr::mutate(
      .pred_conc_mean = mean(
        .data$.pred_conc[which(f_or_na(.data$.is_outlier))],
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()
  list(fit = fit, qp = x)
}
