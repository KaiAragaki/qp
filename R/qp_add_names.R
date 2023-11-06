#' Add sample names
#'
#' @param x A `data.frame` (or a list containing one) that contains columns
#'   `index` (which denotes sample number) and `sample_type`, which should be
#'   either "unknown" or "standard".
#' @param sample_names Optional character vector. If NULL, uses sample index.
#'   In a standard workflow, the index is the order the sample appears in the
#'   plate
#' @param ... Unused
#' @export
qp_add_names <- function(x, ...) {
  UseMethod("qp_add_names")
}

#' @rdname qp_add_names
#' @export
qp_add_names.list <- function(x, sample_names = NULL, ...) {
  x$qp <- qp_add_names(x$qp, sample_names)
  x
}

#' @rdname qp_add_names
#' @export
qp_add_names.data.frame <- function(x, sample_names = NULL, ...) {
  if (!is.null(sample_names)) {
    # Will return "NA" instead of erroring if sample names < # samples
    length(sample_names) <- max(x$index, na.rm = TRUE)
  } else {
    sample_names <- as.character(1:max(x$index, na.rm = TRUE))
  }

  x |> dplyr::mutate(
    .sample_name = ifelse(
      .data$sample_type == "unknown",
      sample_names[.data$index],
      paste("Standard", .data$index)
    ),
    .sample_name = ifelse(
      is.na(.data$.sample_name),
      .data$index,
      .data$.sample_name
    )
  )
}
