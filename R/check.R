check_qp_tidy <- function(x) {
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
  if (any(x < 0)) rlang::warn("Negative values found in .abs")
  if (any(is.na(x))) rlang::warn("NA values found in `.abs`")
}

check_sample_type <- function(x) {
  # NA value are fine
  # They happen when a whole gplate is not used, for instance
  if (!any(grepl("standard", x))) {
    rlang::warn(
      c("`sample_type` does not contain any samples named `standard`",
        "i" = "`sample_type` should denote standards with `standard`")
    )
  }
  if (length(setdiff(x, c("standard", "unknown")) > 0)) {
    rlang::warn(
      c("`sample_type` contains values other than `standard` and `unknown`",
        "!" = "These values may be ignored downstream!")
    )
  }
}

check_is_outlier <- function(x) {
  if (!is.logical(x)) rlang::abort("`.is_outlier` is not `logical`")
}

check_conc <- function(x) {
  if (!is.numeric(x)) rlang::abort("`.conc` is not `numeric`")
  if (any(x < 0)) rlang::abort("`.conc` contains negative values")
}

check_log2_abs <- function(x) {
  if (!is.numeric(x)) rlang::abort("`.conc` is not `numeric`")
}
