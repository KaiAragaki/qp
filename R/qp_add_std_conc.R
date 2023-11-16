#' Add known concentrations of protein to standard samples
#'
#' @param x A `data.frame` containing a `sample_type` and `index` columns.
#'   See details.
#' @param standard_scale A numeric vector giving the concentrations of the
#'   standards. The units are arbitrary, but will determine the units of the
#'   output concentrations.
#' @param ... Unused
#' @details Input is expected to have two columns:
#'   - `sample_type`: A character vector denoting which samples are standards
#'     with "standard". All other values will be considered unknowns.
#'   - `index`: A numeric column denoting the sample number. Index 1 will
#'     correspond to the first item in `standard_scale`, 2 will be the second,
#'     etc.
#' @return Same type as x, with a `.conc` column
#' @export
#' @examples
#' abs <- expand.grid(
#'   sample_type = c("standard", "unknown"),
#'   index = 1:7
#' )
#'
#' abs
#'
#' qp_add_std_conc(abs)
#'
#' # Can add custom scale - doesn't have to be 'in order' or unique:
#' qp_add_std_conc(abs, c(1, 4, 2, 2, 3, 0.125, 7))
#'
#' # Will warn - more values in `standard_scale` than standard indices
#' # Will drop extra
#' qp_add_std_conc(abs, 1:8)
#'
#' # Will error - fewer values in `standard_scale` than standard indices
#' if (FALSE) {
#'   qp_add_std_conc(abs, 1:6)
#' }

qp_add_std_conc <- function(x, standard_scale = c(0, 2^((2:7) - 5)), ...) {
  UseMethod("qp_add_std_conc")
}

#' @rdname qp_add_std_conc
#' @export
qp_add_std_conc.data.frame <- function(x,
                                       standard_scale = c(0, 2^((2:7) - 5)),
                                       ...) {
  check_has_cols(x, c("sample_type", "index"))
  check_sample_type(x$sample_type)

  x$.conc <- NA_real_
  unk <- x[which(x$sample_type != "standard"), ]
  std <- x[which(x$sample_type == "standard"), ]

  check_scale(standard_scale)
  check_std_scale_compat(std, standard_scale)

  std$.conc <- standard_scale[std$index]
  rbind(std, unk)
}

#' @rdname qp_add_std_conc
#' @export
qp_add_std_conc.list <- function(x, standard_scale = c(0, 2^((2:7) - 5)), ...) {
  x$qp <- qp_add_std_conc(x$qp, standard_scale)
  x
}

check_scale <- function(x) {
  if (any(x < 0)) rlang::abort("standards cannot be negative")
}

check_std_scale_compat <- function(std, scale) {
  if (length(unique(std$index)) < length(scale)) {
    rlang::warn("Not all standards in scale used")
  } else if (length(unique(std$index)) > length(scale)) {
    rlang::abort("Length of unique standard indices > `length(standard_scale)`")
  } else if (any(is.na(scale[std$index]))) {
    rlang::abort("standard_scale returned NA when indexed")
  }
}
