library(roxygen2)
library(devtools)
setwd("why-T2.1-pkg")
roxygen2::roxygenize()
devtools::build(
  binary=TRUE
)
devtools::check()
devtools::install(
  upgrade = FALSE,
  quick   = TRUE
  )
setwd("..")
library(whyT2.1)
