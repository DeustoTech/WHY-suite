library(catch22)

for(ii in 1:1E6) {
  catch22::catch22_all(rnorm(1000))
}