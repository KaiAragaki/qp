#' Predict concentrations from standards fit
#' @param x A data.frame. See details.
#' @param fit An `lm` object used to predict concentrations
#' @param ignore_outliers. Boolean. Should outliers be considered when
#' calculating the mean? See details.
#' @param group_cols Character vector. Columns to group by before
#' taking the mean.
#' @details The supplied data.frame should have the following columns:
#'   - Columns used in `fit`. Usually, this is `.log2_conc` and `.log2_abs`
#'   - Any columns in `group_cols`
#'   - If `ignore_outliers = TRUE`, `.is_outlier` will be used if supplied,
#'     or created if not.
#' @return Returns a `tibble` with columns:
#'   - `.pred`: The predicted value from the provided model
#'   - `.pred_conc`: `.pred`, transformed by `conc_transform`
#'   - `.pred_conc_mean`: The mean of `.pred_conc`, sans samples where column
#'     `.is_outlier == TRUE`
#' @export
#' @importFrom rlang .data
qp_calc_conc <- function(x,
                         fit,
                         ignore_outliers = TRUE,
                         group_cols = c("sample_type", "index")) {
  with_predictions <- dplyr::bind_cols(x, .pred = stats::predict(fit, x))
  conc <- with_predictions |>
    dplyr::mutate(.pred_conc = log2(.data$.pred + 0.5)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_cols)))

  if (ignore_outliers) {
    if (!".is_outlier" %in% colnames(conc)) {
      rlang::inform(
        "No colname of `.is_outlier` supplied. Calculating outliers."
      )
      conc <- dplyr::mutate(conc, .is_outlier = mark_outlier(.data$.pred))
    }
    conc <- dplyr::mutate(
      conc,
      .pred_conc_mean = mean(
        .data$.pred_conc[which(f_or_na(.data$.is_outlier))],
        na.rm = TRUE
      )
    )
  } else {
    conc <- dplyr::mutate(
      conc,
      .pred_conc_mean = mean(.data$.pred_conc, na.rm = TRUE)
    )
  }
  conc <- dplyr::ungroup(conc)
  conc
}
