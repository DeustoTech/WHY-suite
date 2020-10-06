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
