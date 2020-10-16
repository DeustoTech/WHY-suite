# Carlos Quesada - Universidad de Deusto
# 2020.07.09
# Generated time series

library(gratis)

gen_ts <- generate_ts_with_target(
  n = 1,
  ts.length = length(values$values),
  freq = c(ts_freq, 7 * ts_freq),
  seasonal = 2,
  features = c("frequency", "stl_features", "entropy", "acf_features"),
  selected.features = names(feats)[1:24],
  target = as.vector(t(as.data.frame(feats[1:24]))),
  parallel = FALSE
)
