#' Create a report for a protein quantificaiton experiment
#'
#' @param qp Likely the output from `qp` AND `qp_dilute`.
#' @param output_file Character. The path of the file to export, including
#'   `.html`
#' @param other Generally used for Shiny application. Assumes a named list of
#'   key-values that will be used to document report parameters.
#'
#' @export
#' @examples
#' \dontrun{
#' absorbances |>
#'   qp() |>
#'   qp_dilute() |>
#'   qp_report(
#'     "~/my_report.html",
#'     other = list(key = "value") # Essentially metadata
#'   )
#' }
qp_report <- function(qp, output_file, other = list()) {
  rmarkdown::render(
    system.file(
      "rmarkdown/templates/quantify-protein-report/skeleton/skeleton.Rmd",
      package = "qp"
    ),
    output_file = output_file,
    params = list(qp = qp, other = other)
  )
}

# Standards ----
## All ----
qp_standards_all <- function(x) {
  UseMethod("qp_standards_all")
}

#' @export
qp_standards_all.data.frame <- function(x) {
  x <- add_coords_col(x)
  x <- add_dilutions_cols(x)
  table_data <- add_well_circles_plot_col(
    x,
    "standard",
    c(".sample_name", ".abs", ".is_outlier", ".pred_conc", ".coord", ".conc",
      "sample_to_add", "add_to", ".target_conc", ".target_vol"),
    c(".sample_name", ".pred_conc", ".conc")
  )
  table_data |>
    make_gt(35, 1, c(".abs", ".pred_conc", ".conc", ".target_conc")) |>
    gt::cols_label(
      .sample_name = "Sample", .conc = "[Actual]",
      .pred_conc = "[Predicted]", .abs = "Absorbance",
      sample_to_add = "Sample to Add", add_to = "Add To",
      .target_conc = "[Target]", .target_vol = "Target Vol.",
      gg = ""
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
  x <- add_coords_col(x)
  table_data <- add_well_circles_plot_col(
    x,
    "standard",
    c(".sample_name", ".pred_conc_mean", ".is_outlier", ".abs", ".coord"),
    c(".sample_name", ".pred_conc_mean"),
    summary = TRUE
  )
  table_data |>
    make_gt(50, 2, ".pred_conc_mean") |>
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
  x <- add_coords_col(x)
  x <- add_dilutions_cols(x)
  table_data <- add_well_circles_plot_col(
    x,
    "unknown",
    c(".sample_name", ".abs", ".is_outlier", ".pred_conc", ".coord",
      "sample_to_add", "add_to", ".target_conc", ".target_vol"),
    group_vars = c(".sample_name", ".pred_conc")
  )
  table_data |>
    make_gt(35, 1, c(".abs", ".pred_conc", ".target_conc")) |>
    gt::cols_label(
      .sample_name = "Sample", .pred_conc = "[Predicted]",
      .abs = "Absorbance",
      sample_to_add = "Sample to Add", add_to = "Add To",
      .target_conc = "[Target]", .target_vol = "Target Vol.",
      gg = ""
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
  x <- add_coords_col(x)
  table_data <- add_well_circles_plot_col(
    x,
    "unknown",
    c(".sample_name", ".pred_conc_mean", ".is_outlier", ".abs", ".coord"),
    c(".sample_name", ".pred_conc_mean"),
    summary = TRUE
  )
  table_data |>
    make_gt(50, 2, ".pred_conc_mean") |>
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

add_dilutions_cols <- function(x) {
  x |>
    dplyr::select(
      -c(".pred_conc_mean", "sample_to_add", "add_to")
    ) |>
    qp_dilute(x$.target_conc, x$.target_vol)
}

add_well_circles_plot_col <- function(x,
                                      sample_type,
                                      select_vars,
                                      group_vars,
                                      summary = FALSE) {
  x <- dplyr::filter(x, .data$sample_type == .env$sample_type)

  x <- x |>
    dplyr::select(dplyr::all_of(select_vars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    tidyr::nest() |>
    dplyr::mutate(gg = purrr::map(.data$data, make_well_circles_plot))

  if (summary) {
    x <- dplyr::select(x, -"data")
  } else {
    x <- x |>
      tidyr::unnest(.data$data) |>
      dplyr::select(-c(".is_outlier", ".coord"))
  }

  dplyr::ungroup(x)
}

add_coords_col <- function(x) {
  x$.coord <- paste0(LETTERS[x$.row], x$.col)
  x
}

make_gt <- function(x, well_img_height, well_img_aspect_ratio, cols_to_round) {
  x |>
    gt::gt() |>
    gt::text_transform(
      location = gt::cells_body(
        columns = .data$gg
      ),
      fn = function(z) {
        gt::ggplot_image(
          x$gg,
          height = gt::px(well_img_height),
          aspect_ratio = well_img_aspect_ratio
        )
      }
    ) |>
    gt::fmt_number(
      dplyr::all_of(cols_to_round),
      n_sigfig = 2
    )
}

dil_summary <- function(qp) {
  check_has_cols(qp, c(
    ".sample_name", ".pred_conc_mean", ".target_conc",
    ".target_vol", "sample_type"
  ))

  qp |>
    dplyr::summarize(
      .by = c(
        ".sample_name", ".pred_conc_mean", ".target_conc",
        ".target_vol", "sample_type"
      )
    ) |>
    qp_dilute() |>
    dplyr::filter(.data$sample_type == "unknown") |>
    dplyr::select(
      Name = ".sample_name",
      ".pred_conc_mean", ".target_conc",
      `Final Vol` = ".target_vol",
      `Sample to Add (uL)` = "sample_to_add",
      `Diluent to Add (uL)` = "add_to"
    ) |>
    dplyr::mutate(
      .target_conc = round(.data$.target_conc, 2),
      .pred_conc_mean = round(.data$.pred_conc_mean, 2)
    ) |>
    dplyr::rename(
      "[Target]" = .data$.target_conc,
      "[Sample]" = .data$.pred_conc_mean
    )
}
