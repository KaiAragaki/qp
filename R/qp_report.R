qp_report <- function(qp) {
  ""
}

# Standards ----
## All ----
qp_standards_all <- function(qp) {
  UseMethod("qp_standards_all")
}

qp_standards_all.data.frame <- function(qp) {
  dplyr::filter(qp$qp, .data$sample_type == "standard")
}

qp_standards_all.list <- function(qp) {
  qp$qp <- qp_standards_all.data.frame(qp$qp)
  qp
}

## Summary ----
qp_standards_summary <- function(qp) {
  UseMethod("qp_standards_summary")
}

qp_standards_summary.data.frame <- function(qp) {
  qp_standards_all(qp) |>
    dplyr::select(
      Name = .data$.sample_name,
      `Known Conc.` = .data$.conc,
      `Mean Predicted Conc.` = .data$.pred_conc_mean
    ) |>
    dplyr::summarise(.by = c("Name", "Known Conc.", "Mean Predicted Conc."))
}

qp_standards_summary.list <- function(qp) {
  qp$qp <- qp_standards_summary.data.frame(qp$qp)
  qp
}


# Samples -----
## All ----
qp_samples_all <- function(qp) {
  UseMethod("qp_samples_all")
}

qp_samples_all.data.frame <- function(qp) {
  dplyr::filter(qp, .data$sample_type == "unknown")
}

qp_samples_all.list <- function(qp) {
  qp$qp <- qp_samples_all.data.frame(qp$qp)
  qp
}

## Summary ----
qp_samples_summary <- function(qp) {
  UseMethod("qp_samples_summary")
}

qp_samples_summary.data.frame <- function(qp) {

}

qp_samples_summary.list <- function(qp) {
  qp$qp <- qp_samples_summary.data.frame(qp$qp)
  qp
}
