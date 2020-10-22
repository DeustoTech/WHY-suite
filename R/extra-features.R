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
  
  browser()
  # Interquartile range
  iqr <- q[[4]] - q[[2]]
  # IQR criterion for outlier detection
  is_outlier <- x < q[[2]] - 1.5 * iqr | x > q[[4]] + 1.5 * iqr
  # Percentage of outliers 
  outlier_pc <- sum(is_outlier) / length(x)
  
  list(
    minimum        = q[[1]],
    lower_quartile = q[[2]],
    median         = q[[3]],
    upper_quartile = q[[4]],
    maximum        = q[[5]],
    iqr            = iqr,
    iqr_outlier_pc = outlier_pc
  )
}

#' Features of electricity
#'
#' @description
#' Compute the load factor across seasonal periods. These seasonal periods are related to days, weeks and years. For days, the selected periods coincide with complete days starting at 00:00.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across seasonal periods. In case there is just one period (and therefore variance cannot be computed), direct load factor for that period is provided.
#' @export

electricity <- function(x) {
  # Get samples per day
  spd            <- attr(x,"msts")[1]
  # Skip omisible samples (those that start or finish in the middle of the day)
  omisible_left  <- (spd - start(x)[2] + 1) %% spd
  omisible_right <- end(x)[2] %% spd
  clean_x        <- x[(1+omisible_left):(length(x)-omisible_right)]
  # Convert TS to matrix of size = 28 days x "spd"
  x_matrix     <- t(matrix(clean_x, nrow=spd))
  # List of load factors (one per day)
  load_factors <- rowMeans(x_matrix)/Rfast::rowMaxs(x_matrix, value=TRUE)
  browser()
  # Results
  if (length(load_factors) == 1) {
    load_factor_1       <- load_factors
  } else {
    mean_load_factors_1 <- mean(load_factors)
    var_load_factors_1  <- stats::var(load_factors)
  }
}
