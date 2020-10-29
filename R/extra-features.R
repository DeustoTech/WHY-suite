################################################################################
# stat_moments
################################################################################

#' Features related to statistical moments
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

################################################################################
# quantiles
################################################################################

#' Features related to quantiles
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

################################################################################
# stat_data_binning
################################################################################

#' Features related to statistical data binning
#' 
#' @description 
#' Compute means and variances of the binned signal, where bins are related to days, weeks and months
#' 
#' @param x Time series of class `msts`.
#'
#' @return A list with statistical values of the different bins.
#' @export

stat_data_binning <- function(x) {
  # Get the seasonal features
  f <- whyT2.1::get_seasonal_features_from_timeseries(x)
  # Output feature list
  o_f <- list()
  
  # Vectors of names for hours
  nmh <- c(
    "mean_00h", "mean_01h", "mean_02h", "mean_03h", "mean_04h", "mean_05h",
    "mean_06h", "mean_07h", "mean_08h", "mean_09h", "mean_10h", "mean_11h",
    "mean_12h", "mean_13h", "mean_14h", "mean_15h", "mean_16h", "mean_17h",
    "mean_18h", "mean_19h", "mean_20h", "mean_21h", "mean_22h", "mean_23h")
  nvh <- c(
    "var_00h", "var_01h", "var_02h", "var_03h", "var_04h", "var_05h",
    "var_06h", "var_07h", "var_08h", "var_09h", "var_10h", "var_11h",
    "var_12h", "var_13h", "var_14h", "var_15h", "var_16h", "var_17h",
    "var_18h", "var_19h", "var_20h", "var_21h", "var_22h", "var_23h")
  numh <- c(
    "unit_mean_00h", "unit_mean_01h", "unit_mean_02h", "unit_mean_03h",
    "unit_mean_04h", "unit_mean_05h", "unit_mean_06h", "unit_mean_07h", 
    "unit_mean_08h", "unit_mean_09h", "unit_mean_10h", "unit_mean_11h", 
    "unit_mean_12h", "unit_mean_13h", "unit_mean_14h", "unit_mean_15h", 
    "unit_mean_16h", "unit_mean_17h", "unit_mean_18h", "unit_mean_19h", 
    "unit_mean_20h", "unit_mean_21h", "unit_mean_22h", "unit_mean_23h")
  nuvh <- c(
    "unit_var_00h", "unit_var_01h", "unit_var_02h", "unit_var_03h",
    "unit_var_04h", "unit_var_05h", "unit_var_06h", "unit_var_07h", 
    "unit_var_08h", "unit_var_09h", "unit_var_10h", "unit_var_11h", 
    "unit_var_12h", "unit_var_13h", "unit_var_14h", "unit_var_15h", 
    "unit_var_16h", "unit_var_17h", "unit_var_18h", "unit_var_19h", 
    "unit_var_20h", "unit_var_21h", "unit_var_22h", "unit_var_23h")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$hourly[,2])
  for (ii in 1:24) {
    o_f[[as.name(nmh[ii])]]  = f$mean$hourly[ii,2]
  }
  for (ii in 1:24) {
    o_f[[as.name(nvh[ii])]]  = f$var$hourly[ii,2]
  }
  for (ii in 1:24) {
    o_f[[as.name(numh[ii])]] = f$mean$hourly[ii,2] / sum_of_means
  }
  for (ii in 1:24) {
    o_f[[as.name(nuvh[ii])]] = f$var$hourly[ii,2] / sum_of_means
  }
  
  # Vectors of names for days
  nmd <- c(
    "mean_sun", "mean_mon", "mean_tue", "mean_wed", "mean_thu", "mean_fri",
    "mean_sat")
  nvd <- c(
    "var_sun", "var_mon", "var_tue", "var_wed", "var_thu", "var_fri",
    "var_sat")
  numd <- c(
    "unit_mean_sun", "unit_mean_mon", "unit_mean_tue", "unit_mean_wed",
    "unit_mean_thu", "unit_mean_fri", "unit_mean_sat")
  nuvd <- c(
    "unit_var_sun", "unit_var_mon", "unit_var_tue", "unit_var_wed",
    "unit_var_thu", "unit_var_fri", "unit_var_sat")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$daily[,2])
  for (ii in 1:7) {
    o_f[[as.name(nmd[ii])]]  = f$mean$daily[ii,2]
  }
  for (ii in 1:7) {
    o_f[[as.name(nvd[ii])]]  = f$var$daily[ii,2]
  }
  for (ii in 1:7) {
    o_f[[as.name(numd[ii])]] = f$mean$daily[ii,2] / sum_of_means
  }
  for (ii in 1:7) {
    o_f[[as.name(nuvd[ii])]] = f$var$daily[ii,2] / sum_of_means
  }
  
  # Vectors of names for months
  nmm <- c(
    "mean_jan", "mean_feb", "mean_mar", "mean_apr", "mean_may", "mean_jun",
    "mean_jul", "mean_ago", "mean_sep", "mean_oct", "mean_nov", "mean_dec")
  nvm <- c(
    "var_jan", "var_feb", "var_mar", "var_apr", "var_may", "var_jun",
    "var_jul", "var_ago", "var_sep", "var_oct", "var_nov", "var_dec")
  numm <- c(
    "unit_mean_jan", "unit_mean_feb", "unit_mean_mar", "unit_mean_apr", 
    "unit_mean_may", "unit_mean_jun", "unit_mean_jul", "unit_mean_ago",
    "unit_mean_sep", "unit_mean_oct", "unit_mean_nov", "unit_mean_dec")
  nuvm <- c(
    "unit_var_jan", "unit_var_feb", "unit_var_mar", "unit_var_apr", 
    "unit_var_may", "unit_var_jun", "unit_var_jul", "unit_var_ago",
    "unit_var_sep", "unit_var_oct", "unit_var_nov", "unit_var_dec")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$monthly[,2])
  for (ii in 1:12) {
    o_f[[as.name(nmm[ii])]]  = f$mean$monthly[ii,2]
  }
  for (ii in 1:12) {
    o_f[[as.name(nvm[ii])]]  = f$var$monthly[ii,2]
  }
  for (ii in 1:12) {
    o_f[[as.name(numm[ii])]] = f$mean$monthly[ii,2] / sum_of_means
  }
  for (ii in 1:12) {
    o_f[[as.name(nuvm[ii])]] = f$var$monthly[ii,2] / sum_of_means
  }
  
  return(o_f)
}

################################################################################
# load_factors
################################################################################

#' Features related to load factors
#'
#' @description
#' Compute the load factor across seasonal periods (days, weeks and years).
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across seasonal periods.
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
  mean_name_list <- c(
    as.name("load_factor_mean1"),
    as.name("load_factor_mean2"),
    as.name("load_factor_mean3")
    )
  var_name_list <- c(
    as.name("load_factor_var1"),
    as.name("load_factor_var2"),
    as.name("load_factor_var3")
  )
  # Seasonality loop
  for (ii in 1:length(attr(x, "msts"))) {
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
    load_factor[[mean_name_list[[ii]]]] <- 
      mean(mean_aggr_ts[2:last_1, 2] / max_aggr_ts[2:last_1, 2])
    load_factor[[var_name_list[[ii]]]]  <- 
      var(mean_aggr_ts[2:last_1, 2] / max_aggr_ts[2:last_1, 2])
  }
  return(load_factor)
}