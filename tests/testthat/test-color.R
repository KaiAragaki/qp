test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("colors are consistent", {
  expect_equal(abs_to_col(0.1, qp_pal), "#A4C4A3")
  expect_equal(abs_to_col(-100, qp_pal), "#A8FF9F")
  expect_equal(abs_to_col(100, qp_pal), "#3D1452")
})

test_that("negative numbers return colors", {
  expect_equal(abs_to_col(-100.31, qp_pal), abs_to_col(-1, qp_pal))
})

test_that("multiple inputs return multiple outputs", {
  expect_equal(abs_to_col(c(0.1, 0.2), qp_pal) |> unique() |> length(), 2)
})
