#' Wrangle qp `gp` according to replicate orientation
#'
#' @param x A `gp`
#' @param replicate_orientation Either 'h' or 'v' - see Details.
#'
#' @details The standards must be in ascending concentration starting in the
#'   upper left corner. Whether this is from from left to right or top to bottom
#'   can be specified in 'replicate orientation'. Note that 'replicate
#'   orientation' specified the direction that REPLICATES lie, NOT the direction
#'   the samples flow (which will be perpendicular to the replicates).
#'
#' @return a `tibble`
#' @export
#' @importFrom rlang .data
qp_tidy <- function(x,
                    replicate_orientation,
                    n_standards = 7,
                    n_replicates = 3,
                    wavelength = 562) {
  max_samples <- gplate::wells(x) %/% n_replicates
  max_unknowns <- max_samples - n_standards
  x <- qp_read(x, wavelength)

  if (replicate_orientation == "v") {
    nrow <- nrow2 <- n_replicates
    ncol <- c(n_standards, max_unknowns)
    flow <- "row"
    ncol2 <- 1
  } else {
    nrow <- c(n_standards, max_unknowns)
    ncol <- ncol2 <- n_replicates
    flow <- "col"
    nrow2 <- 1
  }

  annotated <- x |>
    gplate::gp_sec(
      name = "sample_type", nrow, ncol, wrap = TRUE, flow = flow,
      labels = c("standard", "unknown"), break_sections = FALSE
    ) |>
    gplate::gp_sec(name = "index", nrow2, ncol2, break_sections = FALSE) |>
    gplate::gp_serve()

  annotated$index <- as.numeric(annotated$index)
  annotated |>
    dplyr::arrange(.data$sample_type, .data$index)
}

# TODO User should be allowed to skip this step if index is already provided
# TODO Perhaps a name more evocative than 'index' would be good
