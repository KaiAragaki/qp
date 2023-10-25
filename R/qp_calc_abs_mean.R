#' Calculate absorbance means with optional outlier removal
#'
#' @param x A `data.frame`. See details.
#' @param ignore_outliers Character. Which sample types should have outliers
#' excluded from their mean calculations?
#' @importFrom rlang .data
#'
#' @details The supplied `data.frame` must include the following columns:
#' - `sample_type`, which should contain only either "standard" or "unknown"
qp_calc_abs_mean <- function(x,
                             ignore_outliers =
                               c("all", "standards", "samples", "none")) {
  exclude_outliers <- rlang::arg_match(exclude_outliers)
  standards <- x |>
    dplyr::filter(.data$sample_type == "standard") |>
    calc_mean(exclude_outliers %in% c("all", "standards"))
  unknowns <- x |>
    dplyr::filter(.data$sample_type == "unknown") |>
    calc_mean(exclude_outliers %in% c("all", "samples"))
  rbind(standards, unknowns)
}

calc_mean <- function(df, exclude_outliers) {
  df <- dplyr::group_by(df, .data$sample_type, .data$index)
  if (exclude_outliers) {
    df <- df |>
      dplyr::mutate(
        .is_outlier = mark_outlier(.data$.abs),
        .mean = mean(.data$.abs[!.data$.is_outlier], na.rm = TRUE)
      )
  } else {
    df <- df |>
      dplyr::mutate(
        .is_outlier = NA,
        .mean = mean(.data$.abs, na.rm = TRUE)
      )
  }
  df <- dplyr::ungroup(df)
  df
}

mark_suspect <- function(nums) {
  # Marking a suspect with 2 or fewer samples doesn't make sense
  na_index <- which(!is.na(nums))
  no_na <- na.omit(nums)
  if (length(no_na) <= 2) return(rep(FALSE, length(nums)))
  hc <- stats::hclust(stats::dist(no_na))
  no_na_index <- abs(hc$merge[length(no_na) -1, 1])
  suspect_index <- na_index[no_na_index]
  out <- rep(FALSE, length(nums))
  out[suspect_index] <- TRUE
  out
}

mark_outlier <- function(nums) {
  marked <- mark_suspect(nums)
  if (!any(marked)) return(marked)
  no_suspect <- nums[!marked]
  suspect <- nums[marked]
  mean_no_suspect <- mean(no_suspect, na.rm = TRUE)
  sd_no_suspect <- sd(no_suspect, na.rm = TRUE)
  suspect_is_outlier <- abs(suspect - mean_no_suspect) > (3 * sd_no_suspect)
  if (suspect_is_outlier) {
    return(marked)
  } else {
    return(rep(FALSE, length(nums)))
  }
}