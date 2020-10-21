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
#' Compute the minimum, lower quartile, median, upper quartile and maximum of a time series. Also the Q3 to Q1 range.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the minimum, lower quartile, median, upper quartile, maximum and Q3-Q1 range of the time series.
#'
#' @export

quantiles <- function(x) {
  q <- stats::quantile(x)
  list(
    minimum        = q[[1]],
    lower_quartile = q[[2]],
    median         = q[[3]],
    upper_quartile = q[[4]],
    maximum        = q[[5]],
    q3_to_q1_range = q[[4]] - q[[2]]
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
  spd            <- frequency(x)
  # Skip omisible samples (those that don't start or finish with the day)
  omisible_left  <- (spd - start(x)[2] + 1) %% spd
  omisible_right <- end(x)[2] %% spd
  clean_x        <- x[(1+omisible_left):(length(x)-omisible_right)]
  # Convert TS to matrix of size = 28 days x "spd"
  x_matrix       <- t(matrix(clean_x, nrow=spd))
  # List of load factors (one per day)
  load_factors   <- rowMeans(x_matrix)/Rfast::rowMaxs(x_matrix, value=TRUE)
  # Return
  list(
    mean_load_factors = mean(load_factors),
    var_load_factors  = stats::var(load_factors)
  )
}
