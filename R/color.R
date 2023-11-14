qp_pal <- grDevices::colorRampPalette(
  c("#a8ff9f", "#9e67ab", "#975ab6", "#3D1452"),
  bias = 0.5
)(100)

# Notes:
# Abs vs apparent color
# 0.1 - gray - between green and purple
# 0.31 - around mediumorchid3
# Let's call 0.5 the max since you shouldn't go too far above anyway

abs_to_col <- function(abs, pal) {
  abs <- abs - 0.07
  scaled <- 100 / (1 + exp(-10 * (abs - 0.15)))
  idx <- round(scaled)
  pal[idx]
}

color_test <- function(pal) {
  tibble::tibble(
    abs = seq(from = 0, to = 1, by = 0.025),
    col = abs_to_col(abs, pal)
  ) |>
    ggplot2::ggplot(ggplot2::aes(abs, y = 1, color = col)) +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_color_identity()
}
