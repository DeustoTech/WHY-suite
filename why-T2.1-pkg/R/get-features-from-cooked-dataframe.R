#' Features of a cooked dataframe
#'
#' @description
#' Get features of a cooked dataframe.
#'
#' @param df Cooked dataframe.
#' @param type_of_analysis A string indicating the type of analysis: either `basic` or `extra`. `basic` contains 7 functions whereas `extra` contains 33.
#'
#' @return List of features.
#'
#' @export

get_features_from_cooked_dataframe <- function(df, type_of_analysis) {
  # Set a seed for random numbers
  set.seed(1981)
  # Analysis list
  not_norm_fns <- c("stat_moments", "quantiles", "electricity")
  basic_fns <- c("frequency", "stl_features", "entropy", "acf_features")
  extra_fns <- c("max_kl_shift", "outlierinclude_mdrmd", "arch_stat",
                 "max_level_shift", "ac_9", "crossing_points", "max_var_shift",
                 "nonlinearity", "spreadrandomlocal_meantaul", "flat_spots",
                 "pacf_features", "firstmin_ac", "std1st_der", "heterogeneity",
                 "stability", "firstzero_ac", "trev_num", "holt_parameters",
                 "walker_propcross", "hurst", "unitroot_kpss",
                 "histogram_mode", "unitroot_pp", "localsimple_taures",
                 "lumpiness", "motiftwo_entro3")
  analysis_fns <- list(basic = basic_fns,
                       extra = c(basic_fns, extra_fns))
  # Convert to time series
  vals_msts <- forecast::msts(
    data             = df$df[,2],
    seasonal.periods = df$seasonal_periods,
    ts.frequency     = df$seasonal_periods[1],
    start            = c(1, 1)
  )
  # Extract features that DON'T require normalization of the time series
  not_norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(vals_msts),
    features  = not_norm_fns,
    scale     = FALSE,
    na.action = forecast::na.interp
  )
  # Extract features that REQUIRE normalization of the time series
  norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(vals_msts),
    features  = analysis_fns[[type_of_analysis]],
    scale     = TRUE,
    na.action = forecast::na.interp
  )
  # Bind features into a unique dataframe
  feats <- dplyr::bind_cols(not_norm_feats, norm_feats)
  return(feats)
}
