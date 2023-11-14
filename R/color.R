qp_pal <- grDevices::colorRampPalette(
  c("darkseagreen1", "#B7C7B5", "#A763A0", "#3D1452"),
  bias = 1.5
)(100)

# Notes:
# Abs vs apparent color
# 0.1 - gray - between green and purple
# 0.31 - around mediumorchid3
# Let's call 0.5 the max since you shouldn't go too far above anyway

abs_to_col <- function(abs, pal) {
  scaled <- abs * 200
  idx <- ifelse(scaled < 1, 1, scaled)
  idx <- ifelse(idx > 100, 100, idx)
  idx <- round(idx)
  pal[idx]
}
