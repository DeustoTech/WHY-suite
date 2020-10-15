#' Features of statistical moments
#'
#' @description
#' Compute the mean, variance, skewness and kurtosis of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean, variance, skewness and kurtosis of the time series.
#'
#' @export

stat_moments <- function(x) {
  list(
    mean     = mean(x),
    variance = stats::var(x),
    skewness = moments::skewness(x),
    kurtosis = moments::kurtosis(x)
  )
}

#' Features of quantiles
#'
#' @description
#' Compute the minimum, lower quartile, median, upper quartile and maximum of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the minimum, lower quartile, median, upper quartile and maximum of the time series.
#'
#' @export

quantiles <- function(x) {
  q <- stats::quantile(x)
  list(
    minimum        = q[[1]],
    lower_quartile = q[[2]],
    median         = q[[3]],
    upper_quartile = q[[4]],
    maximum        = q[[5]]
  )
}

#' Features of electricity
#'
#' @description
#' Compute the load factor across 24-hour periods.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across 24-hour periods.
#' @export

electricity <- function(x) {
  # Get samples per day
  samples_per_day <- attr(x, "msts")[1]
  # Pad with NA
  samples_to_pad  <- samples_per_day[1] - (length(x) %% samples_per_day[1])
  padded_x        <- c(as.numeric(x), rep(0, samples_to_pad))
  # Convert TS to matrix of 28 days x "samples_per_day"
  x_matrix        <- t(matrix(padded_x, nrow=samples_per_day))
  # List of load factors (one per day)
  load_factors    <- rowMeans(x_matrix)/Rfast::rowMaxs(x_matrix, value=TRUE)
  # Return
  list(
    mean_load_factors = mean(load_factors),
    var_load_factors  = stats::var(load_factors)
  )
}
