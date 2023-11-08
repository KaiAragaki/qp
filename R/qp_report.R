qp_report <- function(qp) {

}

# Standards ----
## All ----
qp_standards_all <- function(x) {
  UseMethod("qp_standards_all")
}

#' @export
qp_standards_all.data.frame <- function(x) {
  stds <- dplyr::filter(x, .data$sample_type == "standard")
  stds$.coord <- paste0(LETTERS[stds$.row], stds$.col)

  table_data <- stds |>
    dplyr::select(
      ".sample_name", ".abs", ".is_outlier", ".pred_conc", ".coord", ".conc",
    ) |>
    dplyr::group_by(.data$.sample_name, .data$.pred_conc, .data$.conc) |>
    tidyr::nest() |>
    dplyr::mutate(gg = purrr::map(.data$data, make_well_circles_plot)) |>
    tidyr::unnest(.data$data) |>
    dplyr::select(-c(".is_outlier", ".coord")) |>
    dplyr::ungroup()

  table_data |>
    gt::gt() |>
    gt::text_transform(
      location = gt::cells_body(
        columns = .data$gg
      ),
      fn = function(x) {
        gt::ggplot_image(table_data$gg, height = gt::px(35), aspect_ratio = 1)
      }
    ) |>
    gt::fmt_number(
      c(".abs", ".pred_conc", ".conc"),
      n_sigfig = 2
    ) |>
    gt::cols_label(
      .sample_name = "Sample", .conc = "[Actual]",
      .pred_conc = "[Predicted]", .abs = "Absorbance", gg = ""
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
  samples <- dplyr::filter(x, .data$sample_type == "unknown")
  samples$.coord <- paste0(LETTERS[samples$.row], samples$.col)

  table_data <- samples |>
    dplyr::select(
      ".sample_name", ".abs", ".is_outlier", ".pred_conc", ".coord"
    ) |>
    dplyr::group_by(.data$.sample_name, .data$.pred_conc) |>
    tidyr::nest() |>
    dplyr::mutate(gg = purrr::map(.data$data, make_well_circles_plot)) |>
    tidyr::unnest(.data$data) |>
    dplyr::select(-c(".is_outlier", ".coord")) |>
    dplyr::ungroup()

  table_data |>
    gt::gt() |>
    gt::text_transform(
      location = gt::cells_body(
        columns = .data$gg
      ),
      fn = function(x) {
        gt::ggplot_image(table_data$gg, height = gt::px(35), aspect_ratio = 1)
      }
    ) |>
    gt::fmt_number(
      c(".abs", ".pred_conc"),
      n_sigfig = 2
    ) |>
    gt::cols_label(
      .sample_name = "Sample", .pred_conc = "[Predicted]",
      .abs = "Absorbance", gg = ""
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
  samples <- dplyr::filter(x, .data$sample_type == "unknown")
  samples$.coord <- paste0(LETTERS[samples$.row], samples$.col)

  table_data <- samples |>
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
qp_samples_summary.list <- function(x) {
  x$qp <- qp_samples_summary.data.frame(x$qp)
  x
}

make_well_circles_plot <- function(df) {
  x <- seq_along(df$.abs)
  y <- 1
  if (nrow(df) == 1) {
    size <- 120
  } else {
    size <- 200 / nrow(df)
  }

  well <- data.frame(x = x, y = y, color = abs_to_col(df$.abs, qp_pal))

  text <- data.frame(
    x = x, y = y, color = ifelse(df$.abs > 0.3, "white", "black"),
    label = df$.coord
  )

  cross <- data.frame(
    x = x, y = y, color = ifelse(df$.is_outlier, "red", "#FFFFFF00")
  )

  ggplot2::ggplot(
    well, ggplot2::aes(.data$x, .data$y, color = .data$color)
  ) +
    ggplot2::geom_point(size = size, shape = 16) +
    ggplot2::geom_text(
      data = text, ggplot2::aes(label = .data$label), size = size / 2.5
    ) +
    ggplot2::geom_point(data = cross, shape = 4, size = size, stroke = 4) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_color_identity() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 1.5, 0, 1.5, unit = "cm"))
}
