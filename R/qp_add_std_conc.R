#' Add known concentrations of protein to standard samples
#' @param x A `data.frame` containing a `sample_type` and `index` columns.
#' See details.
#' @param standard_scale A numeric vector giving the concentrations of the
#' standards. The units are arbitrary, but will determine the units of the
#' output concentrations.
#' @details Input is expected to have two columns:
#' - `sample_type`: A character vector denoting which samples are standards
#' with "standard". All other values will be considered unknowns.
#' - `index`: A numeric column denoting the sample number. Index 1 will
#' correspond to the first item in `standard_scale`, 2 will be the second, etc.
#' @return Same type as x, with a `.conc` column
#' @export
qp_add_std_conc <- function(x, standard_scale = c(0, 2^((1:7) - 5))) {
  x$.conc <- NA_real_
  unk <- x[which(x$sample_type != "standard"), ]
  std <- x[which(x$sample_type == "standard"), ]

  check_scale(standard_scale)
  check_std_scale_compat(std, standard_scale)

  std$.conc <- standard_scale[std$index]
  rbind(std, unk)
}

check_scale <- function(x) {
  if (any(x < 0)) rlang::abort("standards cannot be negative")
}

check_std_scale_compat <- function(std, scale) {
  if (any(is.na(scale[std$index])))
    rlang::abort("standard_scale returned NA when indexed")

  if (length(unique(std$index)) < length(scale)) {
    rlang::warn("Not all standards in scale used")
  } else if (length(unique(std$index)) > length(scale)) {
    rlang::abort(
      "Length of unique standard incides exceeds length of standard_scale"
    )
  }
}

# TODO should probably add standard scale arg to qp too, where magic
# number 7's length is based on the lenth of standard scale
