## code to prepare `absorbances` dataset goes here
library(mop)
library(qp)

absorbances <- qp_tidy("~/Desktop/abs.txt", replicate_orientation = "v")

usethis::use_data(absorbances, overwrite = TRUE)
