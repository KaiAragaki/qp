test_that("Correct number suspects marked", {
  expect_true(any(mark_suspect(c(1, 1, 1))))
  # Will not mark suspects on fewer than 3 samples
  expect_true(!any(mark_suspect(c(1, 1))))
  expect_true(!any(mark_suspect(c(1))))
  # Ignore NAs
  expect_true(!any(mark_suspect(c(1, NA))))
  expect_true(!any(mark_suspect(c(1, NA, NA))))
  expect_true(!any(mark_suspect(c(1, 1, NA))))
  expect_true(any(mark_suspect(c(1, 1, 1, NA))))
})

test_that("Outliers marked", {
  ins <- 1:2
  # Boundary
  expect_equal(
    mark_outlier(c(ins, mean(ins) + sd(ins) * 3)),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    mark_outlier(c(ins, (mean(ins) + sd(ins) * 3) + 0.001)),
    c(FALSE, FALSE, TRUE)
  )
  expect_equal(mark_outlier(c(1, 1000)), c(FALSE, FALSE))
  expect_equal(mark_outlier(c(1, 1, 1)), c(FALSE, FALSE, FALSE))
})

test_that("Outliers ignored when not checked", {
  vals <- c(1, 1, 100)
  df <- data.frame(sample_type = "standard", index = 1, value = vals)
  expect_equal(calc_mean(df, FALSE)$mean, rep(mean(vals), length(vals)))
  expect_equal(calc_mean(df, TRUE)$mean, rep(1, length(vals)))
})
