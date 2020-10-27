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
  # Compute quantiles
  q <- stats::quantile(x)
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

#' Features of load factors
#'
#' @description
#' Compute the load factor across seasonal periods. These seasonal periods are related to days, weeks and years. For days, the selected periods coincide with complete days starting at 00:00.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across seasonal periods. In case there is just one period (and therefore variance cannot be computed), direct load factor for that period is provided.
#' @export

load_factors <- function(x) {
  # Initial date
  ini_date <- get_extrema_dates_from_timeseries(x)
  # Date sequence
  samples_per_day <- attr(x, "msts")[1]
  date_by  <- as.difftime(24 / samples_per_day, units = "hours")
  date_seq <- seq(from       = ini_date,
                  length.out = length(x),
                  by         = date_by)
  
  # Loop initializations
  cut_breaks_list <- c("1 day", "1 week", "1 month")
  load_factor <- list()
  name_list <- c(as.name("daily"), as.name("weekly"), as.name("monthly"))
  # Seasonality loop
  for (ii in 1:3) {
    ### Bin by periods of 1 month
    cut_seq <- cut(date_seq, breaks = cut_breaks_list[ii])
    # Aggregate data (mean) according to the bins
    mean_aggr_ts <- stats::aggregate(
      x   = as.numeric(x),
      by  = list(date_time = cut_seq),
      FUN = mean)
    # Aggregate data (max) according to the bins
    max_aggr_ts  <- stats::aggregate(
      x   = as.numeric(x),
      by  = list(date_time = cut_seq),
      FUN = max)
    # Compute seasonal mean load factor excluding first and last bins
    last_1 <- dim(max_aggr_ts)[1] - 1
    load_factor[[name_list[[ii]]]] <- 
      mean(mean_aggr_ts[2:last_1, 2] / max_aggr_ts[2:last_1, 2])
  }
  return(load_factor)
}