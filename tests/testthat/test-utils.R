test_that("f_or_na marks correctly", {
  expect_true(f_or_na(FALSE))
  expect_true(f_or_na(NA))
  expect_false(f_or_na(TRUE))
})
