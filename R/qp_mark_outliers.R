#' Mark absorbance outliers
#'
#' @param x A `data.frame` or `list` containing a `data.frame` named `qp`.
#'   See details.
#' @param ignore_outliers Which sample types should have outliers marked?
#'
#' @details Input `data.frame` must contain the following columns:
#' - `sample_type`. Character. Must contain values either "standard" or
#'   "unknown"
#' - `index`. Numeric. Denotes sample number.
#' - `.abs`. Numeric. Contains absorbance values.
#'
#' @return The input `tibble` with an `.is_outlier` column
#'
#' @importFrom rlang .data
#' @export
qp_mark_outliers <- function(x,
                             ignore_outliers = c(
                               "all", "standards", "samples", "none"
                             )) {
  UseMethod("qp_mark_outliers")
}

#' @rdname qp_mark_outliers
#' @export
qp_mark_outliers.data.frame <- function(x,
                                        ignore_outliers = c(
                                          "all", "standards", "samples", "none"
                                        )) {
  ignore_outliers <- rlang::arg_match(ignore_outliers)
  check_has_cols(x, c("sample_type", "index", ".abs"))
  check_sample_type(x$sample_type)
  check_index(x$index)
  check_abs(x$.abs)

  standards <- dplyr::filter(x, .data$sample_type == "standard")
  unknowns <- dplyr::filter(x, .data$sample_type == "unknown")

  if (ignore_outliers %in% c("all", "standards")) {
    standards <- standards |>
      dplyr::mutate(
        .is_outlier = mark_outlier(.data$.abs),
        .by = c("sample_type", "index")
      )
  } else {
    standards$.is_outlier <- NA
  }

  if (ignore_outliers %in% c("all", "samples")) {
    unknowns <- unknowns |>
      dplyr::mutate(
        .is_outlier = mark_outlier(.data$.abs),
        .by = c("sample_type", "index")
      )
  } else {
    unknowns$.is_outlier <- NA
  }

  rbind(standards, unknowns)
}

#' @rdname qp_mark_outliers
#' @export
qp_mark_outliers.list <- function(x,
                                  ignore_outliers = c(
                                    "all", "standards", "samples", "none"
                                  )) {
  ignore_outliers <- rlang::arg_match(ignore_outliers)
  x$qp <- qp_mark_outliers(x$qp, ignore_outliers)
  x
}

mark_outlier <- function(nums) {
  marked <- mark_suspect(nums)
  if (!any(marked)) return(marked)
  no_suspect <- nums[!marked]
  suspect <- nums[marked]
  mean_no_suspect <- mean(no_suspect, na.rm = TRUE)
  sd_no_suspect <- stats::sd(no_suspect, na.rm = TRUE)
  suspect_is_outlier <- abs(suspect - mean_no_suspect) > (3 * sd_no_suspect)
  if (suspect_is_outlier) {
    return(marked)
  } else {
    return(rep(FALSE, length(nums)))
  }
}

mark_suspect <- function(nums) {
  na_index <- which(!is.na(nums))
  no_na <- stats::na.omit(nums)
  # Marking a suspect with 2 or fewer samples doesn't make sense
  if (length(no_na) <= 2) return(rep(FALSE, length(nums)))
  hc <- stats::hclust(stats::dist(no_na))
  no_na_index <- abs(hc$merge[length(no_na) - 1, 1])
  suspect_index <- na_index[no_na_index]
  out <- rep(FALSE, length(nums))
  out[suspect_index] <- TRUE
  out
}
