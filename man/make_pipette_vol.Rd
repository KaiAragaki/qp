% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{make_pipette_vol}
\alias{make_pipette_vol}
\title{Round volume to be pipette-compatible}
\usage{
make_pipette_vol(x)
}
\arguments{
\item{x}{Numeric. Volume to be rounded}
}
\value{
Numeric. Rounded volume.
}
\description{
Round volume to be pipette-compatible
}
\examples{
make_pipette_vol(104.13398)
make_pipette_vol(15.3331)
make_pipette_vol(9.9211)
# Vectorized:
make_pipette_vol(c(104.13398, 15.3331, 9.9211, NA, -100.1))
}
