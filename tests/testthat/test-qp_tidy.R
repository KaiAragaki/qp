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

test_that("checks catch abberent spectramax data", {
  # Can only operate on wavelengths the data have
  abs <- system.file("extdata", "absorbances.txt", package = "qp") |>
    mop::read_spectramax()
  expect_error(
    qp_tidy(abs, wavelength = 561),
    "Specified wavelength not present in data"
  )
  expect_no_error(
    qp_tidy(abs, wavelength = 562),
  )
  # Can't operate on multiplate data
  abs_2_plate <- abs
  abs_2_plate$data[[2]] <- abs$data[[1]]
  expect_error(
    qp_tidy(abs_2_plate),
    "Supplied data does not include 1 \\(and only 1\\) plate"
  )
})

test_that("max_unknowns calculated correctly", {
  expect_equal(get_max_unknowns(80, 3, 7), 19)
  expect_equal(get_max_unknowns(2, 2, 1), 0)
  expect_equal(get_max_unknowns(3, 2, 1), 0)
  expect_equal(get_max_unknowns(0, 2, 2), 0)
})
