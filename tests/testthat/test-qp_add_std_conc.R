test_that("index matches order", {
  x <- data.frame(sample_type = "standard", index = c(3, 2, 1, 1, 3))
  a <- qp_add_std_conc(x, standard_scale = c(1, 2, 3))
  b <- qp_add_std_conc(x, standard_scale = c(1, 3, 2))
  expect_equal(a$.conc, c(3, 2, 1, 1, 3))
  expect_equal(b$.conc, c(2, 3, 1, 1, 2))
})

test_that("standard and scale length match", {
  x <- data.frame(sample_type = "standard", index = 1:2)
  y <- data.frame(sample_type = "standard", index = 2:3)
  expect_error(qp_add_std_conc(x, standard_scale = 1))
  expect_warning(qp_add_std_conc(x, standard_scale = 1:3))
  expect_error(qp_add_std_conc(y, standard_scale = 1:2))
})

test_that("fails on malformed scale", {
  expect_error(check_scale(c(-1, 1, 10)))
})

test_that("succeeds when no unknowns provided", {
  x <- data.frame(sample_type = "standard", index = 1)
  a <- qp_add_std_conc(x, standard_scale = 1)
  expect_no_error(a)
})
