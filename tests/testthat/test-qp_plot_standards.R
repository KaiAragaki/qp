test_that("qp_plot_standards works", {
  suppressWarnings(out <- qp(absorbances) |> qp_plot_standards())
  expect_true(inherits(out, "ggplot"))
})
