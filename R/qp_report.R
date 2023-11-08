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
    dplyr::mutate(x, .row = LETTERS[.data$.row]) |>
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
  stds <- dplyr::filter(x, .data$sample_type == "standard")
  stds$.coord <- paste0(LETTERS[stds$.row], stds$.col)

  table_data <- stds |>
    dplyr::select(
      ".sample_name", ".pred_conc_mean", ".is_outlier", ".abs", ".coord"
    ) |>
    dplyr::group_by(.data$.sample_name, .data$.pred_conc_mean) |>
    tidyr::nest() |>
    dplyr::mutate(gg = purrr::map(.data$data, make_well_circles_plot)) |>
    dplyr::select(-.data$data) |>
    dplyr::ungroup()

  table_data |>
    gt::gt() |>
    gt::text_transform(
      location = gt::cells_body(
        columns = .data$gg
      ),
      fn = function(x) {
        gt::ggplot_image(table_data$gg, height = gt::px(50), aspect_ratio = 2)
      }
    ) |>
    gt::fmt_number(
      c(".pred_conc_mean"),
      drop_trailing_zeros = TRUE,
      n_sigfig = 2
    ) |>
    gt::cols_label(
      .sample_name = "Sample", .pred_conc_mean = "Concentration", gg = ""
    )
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
    dplyr::mutate(x, .row = LETTERS[.data$.row]) |>
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

make_well_circles_plot <- function(x) {
  well <- data.frame(
    x = seq_along(x$.abs),
    y = 1,
    color = abs_to_col(x$.abs, qp_pal)
  )

  text <- data.frame(
    x = seq_along(x$.abs),
    y = 1,
    color = ifelse(x$.abs > 0.3, "white", "black"),
    label = x$.coord
  )

  cross <- data.frame(
    x = seq_along(x$.abs),
    y = 1,
    color = ifelse(x$.is_outlier, "red", "#FFFFFF00")
  )


  ggplot2::ggplot(
    well,
    ggplot2::aes(.data$x, .data$y, color = .data$color)
  ) +
    ggplot2::geom_point(size = 200/nrow(x), shape = 16) +
    ggplot2::geom_text(data = text, aes(label = label), size = 200/nrow(x)/2) +
    ggplot2::geom_point(data = cross, shape = 4, size = 200/nrow(x), stroke = 4) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_color_identity() +
    ggplot2::theme(plot.margin = margin(0, 1.5, 0, 1.5, unit = "cm"))
}
