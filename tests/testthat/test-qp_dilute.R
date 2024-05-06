test_that("length_is_recyclable works", {
  expect_true(length_is_recyclable(1, iris[1, ]))
  expect_true(length_is_recyclable(1, iris[1:2, ]))
  expect_true(length_is_recyclable(2, iris[1:2, ]))
  expect_true(length_is_recyclable(0, iris[1:2, ]))
  expect_false(length_is_recyclable(2, iris))
})

test_that("dilution prefers .mean_pred_conc", {
  data <- data.frame(
    .pred_conc = c(1, 2),
    .pred_conc_mean = c(5, 10)
  )
  out <- qp_dilute(data, target_conc = 5, target_vol = 2)
  expect_equal(out$sample_to_add, c(2, 1))
  expect_equal(out$add_to, c(0, 1))
})

test_that("dilution can use .pred_conc", {
  data <- data.frame(.pred_conc = c(1, 2))
  out <- qp_dilute(data, target_conc = 1, target_vol = 2)
  expect_equal(out$sample_to_add, c(2, 1))
  expect_equal(out$add_to, c(0, 1))
})

test_that("NULL target_conc uses lowest concentration", {
  data <- data.frame(
    sample_type = c("standard", "standard", "unknown", "unknown"),
    .pred_conc = 1:4
  )
  out <- qp_dilute(data, target_vol = 2)
  expect_equal(out$sample_to_add, c(6, 3, 2, 1.5))
  expect_equal(out$add_to, c(-4, -1, 0, 0.5))
})

test_that("output type same regardless of input len", {
  short <- data.frame(.pred_conc = 1) |>
    qp_dilute(target_conc = 1, target_vol = 2)
  long <- data.frame(.pred_conc = c(1, 2)) |>
    qp_dilute(target_conc = 1, target_vol = 2)
  expect_equal(class(short), class(long))
})
