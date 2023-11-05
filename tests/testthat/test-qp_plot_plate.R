test_that("qp_plot_plate works", {
  expect_true(inherits(qp_plot_plate(absorbances), "ggplot"))
})
