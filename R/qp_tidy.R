#' Read in and wrangle protein quantification data
#'
#' @param x A `gp`, `data.frame`/`tibble`, `spectramax`, or character
#' path to a raw SPECTRAmax .xls(x)/.txt
#' @param replicate_orientation Character. Specified the direction the
#'   *replicates* lie, not the direction the samples flow (which will be
#'   perpendicular to `replicate_orientation`).
#' @param n_standards Numeric. The number of different concentrations of
#'   standards. Does not include replicates.
#' @param n_replicates Numeric. The number of replicates per sample.
#' @param wavelength Numeric. For SPECTRAmax files and objects, the
#'   wavelength measured. Otherwise, ignored.
#' @param ... Arguments passed to relevant methods.
#
#' @details `qp` assumes that if you read in data not in a `spectramax` file
#'   or object, you probably have a custom workflow in mind - therefore, tidying
#'   will be minimal and mostly focused on checking for validity.
#'
#' @return a `data.frame`
#' @export
#' @importFrom rlang .data
qp_tidy <- function(x, ...) {
  UseMethod("qp_tidy")
}

#' @export
#' @rdname qp_tidy
qp_tidy.character <- function(x, ...) {
  x <- mop::read_spectramax(x)
  qp_tidy(x, ...)
}

#' @export
#' @rdname qp_tidy
qp_tidy.spectramax <- function(x,
                               replicate_orientation = c("h", "v"),
                               n_standards = 7,
                               n_replicates = 3,
                               wavelength = 562,
                               ...) {
  replicate_orientation <- rlang::arg_match(replicate_orientation)

  if (!(wavelength %in% x$wavelengths))
    rlang::abort("Specified wavelength not present in data")

  plate_index <- which(sapply(x$data, \(x) x$type == "Plate"))

  if (length(plate_index) != 1)
    rlang::abort("Supplied data does not include 1 (and only 1) plate")

  x <- x$data[[plate_index]]$data

  max_samples <- gplate::wells(x) %/% n_replicates
  max_unknowns <- max_samples - n_standards

  if (replicate_orientation == "v") {
    nrow <- nrow2 <- n_replicates
    ncol <- c(n_standards, max_unknowns)
    flow <- "row"
    ncol2 <- 1
  } else {
    nrow <- c(n_standards, max_unknowns)
    ncol <- ncol2 <- n_replicates
    flow <- "col"
    nrow2 <- 1
  }
  x <- x |>
    gplate::gp_sec(
      name = "sample_type", nrow, ncol, wrap = TRUE, flow = flow,
      labels = c("standard", "unknown"), break_sections = FALSE
    ) |>
    gplate::gp_sec(name = "index", nrow2, ncol2, break_sections = FALSE) |>
    gplate::gp_serve()

  x$index <- as.numeric(x$index)
  x <- x |>
    dplyr::rename(.abs = paste0("nm", wavelength)) |>
    dplyr::arrange(.data$sample_type, .data$index)
  qp_tidy(x, ...)
}


#' @rdname qp_tidy
#' @export
qp_tidy.gp <- function(x, ...) {
  x <- gplate::gp_serve(x)
  qp_tidy(x, ...)
}

#' @rdname qp_tidy
#' @export
qp_tidy.default <- function(x, ...) {
  check_validity(x)
  x
}

check_validity <- function(x) {
  check_has_cols(x, c("sample_type", "index", ".abs"))
  check_sample_type(x$sample_type)
  check_abs(x$.abs)
}

check_has_cols <- function(df, cols) {
  if (!all(cols %in% colnames(df))) {
    missing <- setdiff(cols, colnames(df))
    cli::cli_abort(c(
      "Data is missing columns {.code {missing}}"
    ))
  }
}

check_index <- function(x) {
  # NA value are fine
  # They happen when a whole gplate is not used, for instance
  if (class(x) != "numeric") rlang::abort("index must be numeric")
}

check_abs <- function(x) {
  if (class(x) != "numeric") rlang::abort("abs must be numeric")
  if (any(x$.abs < 0)) rlang::warn("Negative values found in .abs")
  if (any(is.na(x$.abs))) rlang::warn("NA values found in `.abs`")
}

check_sample_type <- function(x) {
  # NA value are fine
  # They happen when a whole gplate is not used, for instance
  if (!any(grepl("standard", x$sample_type))) {
    rlang::warn(
      c("`sample_type` does not contain any samples named `standard`",
        "i" = "`sample_type` should denote standards with `standard`")
    )
  }
  if (length(setdiff(x$sample_type, c("standard", "unknown")) > 0)) {
    rlang::warn(
      c("`sample_type` contains values other than `standard` and `unknown`",
        "!" = "These values may be ignored downstream!")
    )
  }
}
