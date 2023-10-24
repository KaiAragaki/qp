#' Read in data into a common quantify protein format

#' This function will perform some very basic checks to see if the data is
#' properly formatted
#' @param x A `gp`, `data.frame`/`tibble`, `spectramax`, or character
#' path to a raw SPECTRAmax .xls(x)/.txt
#' @param wavelength Numeric. For SPECTRAmax files, the wavelength captured.
#' @param ... Unused
#'
#' @return A `gp`
#' @export
qp_read <- function(x, ...) {
  UseMethod("qp_read")
}

#' @export
#' @rdname qp_read
qp_read.character <- function(x, wavelength = 562, ...) {
  mop::read_spectramax(x, ...) |> qp_read(wavelength = wavelength)
}

#' @export
#' @rdname qp_read
qp_read.data.frame <- function(x, ...) {
  gplate::as_gp(x)
}

#' @export
#' @rdname qp_read
qp_read.gp <- function(x, ...) {
  x
}

#' @export
#' @rdname qp_read
qp_read.spectramax <- function(x, wavelength = 562, ...) {
  if (!(wavelength %in% x$wavelengths))
    rlang::abort("Specified wavelength not present in data")

  plate_index <- which(sapply(x$data, \(x) x$type == "Plate"))

  if (length(plate_index) != 1)
    rlang::abort("Supplied data does not include 1 (and only 1) plate")

  x$data[[plate_index]]$data
}

# TODO:
# Perform a suite of basic checks to see if the data have all the
# columns they need
