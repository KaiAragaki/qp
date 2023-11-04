test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that(".log2_abs is calculated if not present", {
  x <- absorbances[1:40, ] |>
    qp_mark_outliers("all") |>
    qp_add_std_conc()
  expect_message(qp_fit(x), "Did not find column `\\.log2_abs`")
  expect_equal(log2(x$.abs), qp_fit(x)$qp$.log2_abs)
})

test_that(".is_outlier is produced if not present", {
  x <- absorbances[1:40, ]
  x$.log2_abs <- log2(x$.abs)
  x <- qp_add_std_conc(x)
  expect_message(qp_fit(x), "Did not find column `\\.is_outlier`")
  expect_true(all(is.na(qp_fit(x)$qp$.is_outlier)))
})
