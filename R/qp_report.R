qp_report <- function(qp) {
  ""
}

# Standards ----
## All ----
qp_standards_all <- function(x) {
  UseMethod("qp_standards_all")
}

#' @export
qp_standards_all.data.frame <- function(x) {
  dplyr::filter(x, .data$sample_type == "standard") |>
    dplyr::select(
      Name = ".sample_name",
      Absorbance = ".abs",
      "Known Conc." = ".conc",
      "Predicted Conc." = ".pred_conc",
      "Mean Predicted Conc." = ".pred_conc_mean",
      Outlier = ".is_outlier",
      Row = ".row",
      Col = ".col"
    )
}

#' @export
qp_standards_all.list <- function(x) {
  x$qp <- qp_standards_all.data.frame(x$qp)
  x
}

## Summary ----
qp_standards_summary <- function(x) {
  UseMethod("qp_standards_summary")
}

#' @export
qp_standards_summary.data.frame <- function(x) {
  qp_standards_all(x) |>
    dplyr::summarize(.by = c("Name", "Known Conc.", "Mean Predicted Conc."))
}

#' @export
qp_standards_summary.list <- function(x) {
  x$qp <- qp_standards_summary.data.frame(x$qp)
  x
}


# Samples -----
## All ----
qp_samples_all <- function(x) {
  UseMethod("qp_samples_all")
}

#' @export
qp_samples_all.data.frame <- function(x) {
  dplyr::filter(x, .data$sample_type == "unknown") |>
    dplyr::select(
      Name = ".sample_name",
      Absorbance = ".abs",
      "Predicted Conc." = ".pred_conc",
      "Mean Predicted Conc." = ".pred_conc_mean",
      Outlier = ".is_outlier",
      Row = ".row",
      Col = ".col"
    )
}

#' @export
qp_samples_all.list <- function(x) {
  x$qp <- qp_samples_all.data.frame(x$qp)
  x
}

## Summary ----
qp_samples_summary <- function(x) {
  UseMethod("qp_samples_summary")
}

#' @export
qp_samples_summary.data.frame <- function(x) {
  qp_samples_all(x) |>
    dplyr::summarize(.by = c("Name", "Mean Predicted Conc."))
}

#' @export
qp_samples_summary.list <- function(x) {
  x$qp <- qp_samples_summary.data.frame(x$qp)
  x
}

make_well_circle <- function(abs) {

}
