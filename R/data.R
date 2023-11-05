#' Absorbances from a protein quantification
#'
#' Absorbances from a BCA protein quantification in a `data.frame`
#'
#' @format ## `absorbances`
#' A `data.frame` with 96 rows and 5 columns:;
#' \describe{
#'   \item{.row}{The row of the 96 well plate, where 1 refers to the top row.}
#'   \item{.col}{The column of the 96 well plate, where 1 refers to the left column.}
#'   \item{.abs}{The absorbance of the contents of the well at 562nm.}
#'   \item{sample_type}{Denotes whether the sample is a standard or an unknown (sample).}
#'   \item{index}{Denotes individual standards/samples, where each gets its own index.}
#' }
#'
"absorbances"
