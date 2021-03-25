library(roxygen2)
library(devtools)
setwd("..")

# roxygen2::roxygenize()
devtools::build()
# devtools::check()
devtools::install(
  upgrade = FALSE,
  quick   = TRUE
)

devtools::load_all()

setwd("scripts")
library(whyT2.1)
