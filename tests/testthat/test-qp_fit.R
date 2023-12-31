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
})

test_that("fit is consistent", {
  suppressMessages(
    suppressWarnings(
      fit <- absorbances[1:40, ] |>
        qp_add_std_conc() |>
        qp_fit()
    )
  )
  coefs <- round(fit$fit$coefficients, 2)
  expect_true(all(coefs == c(2.38, 0.85)))
})
