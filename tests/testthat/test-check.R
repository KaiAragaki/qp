test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("check_has_cols works", {
  df <- data.frame(a = "apple", b = "bee", c = "cow")
  expect_no_error(check_has_cols(df, c("a", "b"), "and"))
  expect_no_error(check_has_cols(df, c("a", "z"), "or"))
  expect_error(check_has_cols(df, c("a", "z"), "and"))
  expect_error(check_has_cols(df, c("z"), "or"))
})

test_that("check_index works", {
  expect_error(check_index("1"))
  expect_no_error(check_index(1))
  expect_no_error(check_index(NA_integer_))
})

test_that("check_abs works", {
  expect_error(check_abs(c("1", 2)), "`\\.abs` must be numeric")
  expect_no_error(check_abs(c(0, 1)))
  expect_warning(check_abs(c(-0.1, 0)), "Negative values found in `\\.abs`")
  expect_error(check_abs(c(0, NA)))
})
