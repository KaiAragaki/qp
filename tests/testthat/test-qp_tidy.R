test_that("checks catch abberent data", {
  x <- data.frame(index = 1, sample_type = "standard", .abs = 0.1)
  has_neg <- data.frame(index = 1, sample_type = "standard", .abs = -0.1)
  has_strange_sample_name <- data.frame(
    index = 1, sample_type = c("standard", "fooby"), .abs = 0.1
  )
  lacks_standard <- data.frame(
    index = 1, sample_type = "unknown", .abs = 0.1
  )
  expect_warning(qp_tidy(has_neg))
  expect_warning(qp_tidy(has_strange_sample_name))
  expect_warning(qp_tidy(lacks_standard))
})

