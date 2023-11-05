test_that("concentration calculation is correct", {
  fit <- absorbances[1:40, ] |> qp_add_std_conc() |> qp_fit()
  pred_qp <- qp_calc_conc(fit)
  pred_manual <- predict(fit$fit, fit$qp)
  expect_equal(pred_manual, pred_qp$qp$.pred)
  expect_equal(2^pred_manual - 0.5, pred_qp$qp$.pred_conc)
})
