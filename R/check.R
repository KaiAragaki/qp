check_qp_tidy <- function(x) {
  check_has_cols(x, c("sample_type", "index", ".abs"))
  check_sample_type(x$sample_type)
  check_abs(x$.abs)
}

check_has_cols <- function(df, cols, type = "and") {
  if (!has_cols(df, cols, type)) {
    missing <- setdiff(cols, colnames(df))
    if (type == "and")
      cli::cli_abort("Data is missing columns {.code {missing}}")
    if (type == "or") {
      cli::cli_abort(
        "Data requires at least one of the following columns: {.code {missing}}"
      )
    }
  }
}

# Returns boolean but doesn't raise problems like `check_has_cols`
has_cols <- function(df, cols, type = "and") {
  if (type == "and") out <- all(cols %in% colnames(df))
  if (type == "or") out <- any(cols %in% colnames(df))
  out
}

check_index <- function(x) {
  # NA value are fine
  # They happen when a whole gplate is not used, for instance
  if (!is.numeric(x)) rlang::abort("index must be numeric")
}

check_abs <- function(x) {
  if (!is.numeric(x)) rlang::abort("abs must be numeric")
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
  if (length(setdiff(x, c("standard", "unknown"))) > 0) {
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
  if (any(x < 0, na.rm = TRUE)) rlang::abort("`.conc` contains negative values")
}

check_log2_abs <- function(x) {
  if (!is.numeric(x)) rlang::abort("`.conc` is not `numeric`")
}

check_pred_conc_mean <- function(x) {
  # It's possible for predicted values to be negative
  if (!is.numeric(x)) rlang::abort("`.pred_conc_mean` is not `numeric`")
}
