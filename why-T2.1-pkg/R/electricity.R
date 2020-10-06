#' Features of electricity
#'
#' @description
#' Compute the load factor across days of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across days in the time series.
#' @export

electricity <- function(x) {
  # Get samples per day
  samples_per_day <- attr(x, "msts")[1]
  # Convert TS to matrix of 28 days x "samples_per_day"
  x_matrix <- t(matrix(x, nrow=samples_per_day))
  # List of load factors (one per day)
  load_factors <- rowMeans(x_matrix)/Rfast::rowMaxs(x_matrix, value=TRUE)
  # Return
  list(
    mean_load_factors = mean(load_factors),
    var_load_factors = stats::var(load_factors)
  )
}
