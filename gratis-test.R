##### Generate diverse time series #############################################
# Load GRATIS
library(gratis)
# Generate time series
x <- generate_ts(n.ts = 1, freq = 12, nComp = 2, n = 120)
# Plot time series
ggplot2::autoplot(x$N1$x)

##### Generate mutiple seasonal time series ####################################
# Load GRATIS
library(gratis)
# Generate time series
x <- generate_msts(seasonal.periods = c(7, 365), n = 800, nComp = 2)
# Plot time series
ggplot2::autoplot(x)

##### Generate time series with controllable features ##########################
# Load GRATIS
library(gratis)
# Generate time series
x <- generate_ts_with_target(n = 1, ts.length = 60, freq = 1, seasonal = 0,
                             features = c('entropy', 'stl_features'),
                             selected.features = c('entropy', 'trend'),
                             target = c(0.6, 0.9))
# Plot time series
ggplot2::autoplot(x)

##### GRATIS generation trial ##################################################
# Load GRATIS
library(gratis)
# Generate time series
x <- generate_ts_with_target(
  n = 1, 
  ts.length = 60, 
  freq = 1, 
  seasonal = 0,
  features = c('entropy', 'stl_features'), 
  selected.features = c('entropy', 'trend'),
  target = c(0.6, 0.9)
)
# Plot time series
ggplot2::autoplot(x)
