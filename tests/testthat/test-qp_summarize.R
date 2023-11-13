test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("qp_summarize summary is correct", {
  df <- expand.grid(
    rep = 1:3,
    sample_type = c("standard", "unknown"),
    index = 1:3
  )
  df$sample_type <- as.character(df$sample_type)
  df$.pred_conc_mean <- 1:18
  out <- data.frame(
    .sample_name = c(paste0("Standard ", 1:3), 1:3),
    sample_type = rep(c("standard", "unknown"), each = 3),
    .pred_conc_mean = c(2, 8, 14, 5, 11, 17)
  )
  expect_equal(qp_summarize.data.frame(df), out)
})
