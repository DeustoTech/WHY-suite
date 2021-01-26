################################################################################
# stat_moments
################################################################################

#' Features related to statistical moments
#'
#' @description
#' Compute the mean, variance, skewness and kurtosis of a time series.
#'
#' @param x Time series of class \code{msts}.
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
#' @param x Time series of class \code{msts}.
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
# load_factors
################################################################################

#' Features related to load factors
#'
#' @description
#' Compute the load factor across seasonal periods (days, weeks and years). A high load factor means power usage is relatively constant. Low load factor shows that occasionally a high demand is set.
#'
#' @param x Time series of class \code{msts}.
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
    avg_aggr_ts  <- stats::aggregate(
      x   = as.numeric(x),
      by  = list(date_time = cut_seq),
      FUN = mean)
    # Aggregate data (max) according to the bins
    max_aggr_ts  <- stats::aggregate(
      x   = as.numeric(x),
      by  = list(date_time = cut_seq),
      FUN = max)
    # Compute seasonal mean load factor excluding first and last bins
    out <- c(-1, -dim(max_aggr_ts)[1])
    avg_over_max <- avg_aggr_ts[out, 2] / max_aggr_ts[out, 2]
    # Turn NaN into 0's (this happens when avg = max = 0)
    avg_over_max[is.na(avg_over_max)] <- 0
    # Load factor means and vars 
    load_factor[[mean_name_list[[ii]]]] <- mean(avg_over_max)
    load_factor[[ var_name_list[[ii]]]] <- stats::var(avg_over_max)
  }
  return(load_factor)
}

################################################################################
# stat_data_aggregates
################################################################################

#' Features related to statistical data aggregates
#' 
#' @description 
#' Compute means and variances of time series aggregates, where bins are days, weeks and months.
#' 
#' @details \code{mean_XXh} and \code{var_XXh}: the mean and variance of the time series at \code{XX} hours, where \code{XX} goes from 00 to 23. \code{unit_mean_XXh} and \code{unit_var_XXh}: the same as before but values of the mean are normalized so that the sum is 1. \code{mean_xxx} and \code{var_xxx}: the mean and variance of the time series on \code{xxx}, where \code{xxx} goes from sun (Sunday) to sat (Saturday). \code{unit_mean_xxx} and \code{unit_var_xxx}: the same as before but values are normalized so that the sum is 1. \code{mean_yyy} and \code{var_yyy}: the mean and variance of the time series in \code{yyy}, where \code{yyy} goes from jan (January) to dec (December). \code{unit_mean_yyy} and \code{unit_var_yyy}: the same as before but values are normalized so that the sum is 1.
#' 
#' @param x Time series of class \code{msts}.
#'
#' @return A list with statistical values of the different bins.
#' @export

stat_data_aggregates <- function(x) {
  # Get the seasonal features
  f <- whyT2.1::get_seasonal_features_from_timeseries(x)
  # Output feature list
  o_f <- list()
  
  # DEFINITION OF SPECIAL FUNCTION TO AVOID NaN WHEN DIVIDING BY 0
  "%/%" <- function(x,y) ifelse(y==0, 0, x/y)
  
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
  nm4h <- c(
    "mean_00h_04h", "mean_04h_08h", "mean_08h_12h",
    "mean_12h_16h", "mean_16h_20h", "mean_20h_00h")
  nv4h <- c(
    "var_00h_04h", "var_04h_08h", "var_08h_12h",
    "var_12h_16h", "var_16h_20h", "var_20h_00h")
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
  num4h <- c(
    "unit_mean_00h_04h", "unit_mean_04h_08h", "unit_mean_08h_12h",
    "unit_mean_12h_16h", "unit_mean_16h_20h", "unit_mean_20h_00h")
  nuv4h <- c(
    "unit_var_00h_04h", "unit_var_04h_08h", "unit_var_08h_12h",
    "unit_var_12h_16h", "unit_var_16h_20h", "unit_var_20h_00h")
  # nint <- c("00h_04h", "04h_08h", "08h_12h", "12h_16h", "16h_20h", "20h_00h")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$hourly[,2])
  sum_of_extra_means <- sum(f$mean$`4-hourly`[,2])
  
  for (ii in 1:24) {
    o_f[[as.name(nmh[ii])]]  = f$mean$hourly[ii,2]
  }
  for (ii in 1:24) {
    o_f[[as.name(nvh[ii])]]  = f$var$hourly[ii,2]
  }
  for (ii in 1:24) {
    o_f[[as.name(numh[ii])]] = f$mean$hourly[ii,2] %/% sum_of_means
  }
  for (ii in 1:24) {
    o_f[[as.name(nuvh[ii])]] = f$var$hourly[ii,2] %/% sum_of_means
  }
  for (ii in 1:6) {
    o_f[[as.name(nm4h[ii])]]  = f$mean$`4-hourly`[ii,2]
  }
  for (ii in 1:6) {
    o_f[[as.name(nv4h[ii])]]  = f$var$`4-hourly`[ii,2]
  }
  for (ii in 1:6) {
    o_f[[as.name(num4h[ii])]] = f$mean$`4-hourly`[ii,2] %/% sum_of_extra_means
  }
  for (ii in 1:6) {
    o_f[[as.name(nuv4h[ii])]] = f$var$`4-hourly`[ii,2] %/% sum_of_extra_means
  }
  
  # for (ii in 6:1) {
  #   if (ii != 1) {
  #     for (jj in (ii-1):1) {
  #       rat_name <- as.name(paste("ratio_", nint[ii], "_to_", nint[jj], sep=""))
  #       top_name <- as.name(paste("mean_", nint[ii], sep=""))
  #       bot_name <- as.name(paste("mean_", nint[jj], sep=""))
  #       o_f[[rat_name]] <- o_f[[top_name]] / o_f[[bot_name]]
  #     }
  #   }
  # }

  # Vectors of names for days
  nmd <- c(
    "mean_sun", "mean_mon", "mean_tue", "mean_wed", 
    "mean_thu", "mean_fri", "mean_sat")
  nvd <- c(
    "var_sun", "var_mon", "var_tue", "var_wed", "var_thu", "var_fri", "var_sat")
  nmwe <- c("mean_weekday", "mean_weekend")
  nvwe <- c("var_weekday", "var_weekend")
  numd <- c(
    "unit_mean_sun", "unit_mean_mon", "unit_mean_tue", "unit_mean_wed",
    "unit_mean_thu", "unit_mean_fri", "unit_mean_sat")
  nuvd <- c(
    "unit_var_sun", "unit_var_mon", "unit_var_tue", "unit_var_wed",
    "unit_var_thu", "unit_var_fri", "unit_var_sat")
  numwe <- c("unit_mean_weekday", "unit_mean_weekend")
  nuvwe <- c("unit_var_weekday", "unit_var_weekend")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$daily[,2])
  sum_of_extra_means <- sum(f$mean$weekends[,2])
  
  for (ii in 1:7) {
    o_f[[as.name(nmd[ii])]]  = f$mean$daily[ii,2]
  }
  for (ii in 1:7) {
    o_f[[as.name(nvd[ii])]]  = f$var$daily[ii,2]
  }
  for (ii in 1:7) {
    o_f[[as.name(numd[ii])]] = f$mean$daily[ii,2] %/% sum_of_means
  }
  for (ii in 1:7) {
    o_f[[as.name(nuvd[ii])]] = f$var$daily[ii,2] %/% sum_of_means
  }

  for (ii in 1:2) {
    o_f[[as.name(nmwe[ii])]]  = f$mean$weekends[ii,2]
  }
  for (ii in 1:2) {
    o_f[[as.name(nvwe[ii])]]  = f$var$weekends[ii,2]
  }
  for (ii in 1:2) {
    o_f[[as.name(numwe[ii])]] = f$mean$weekends[ii,2] %/% sum_of_extra_means
  }
  for (ii in 1:2) {
    o_f[[as.name(nuvwe[ii])]] = f$var$weekends[ii,2] %/% sum_of_extra_means
  }
  
  # o_f[["ratio_weekend_to_weekday"]] <-
  #   o_f[["mean_weekend"]] / o_f[["mean_weekday"]]
  
  # Vectors of names for months
  nmm <- c(
    "mean_jan", "mean_feb", "mean_mar", "mean_apr", "mean_may", "mean_jun",
    "mean_jul", "mean_aug", "mean_sep", "mean_oct", "mean_nov", "mean_dec")
  nvm <- c(
    "var_jan", "var_feb", "var_mar", "var_apr", "var_may", "var_jun",
    "var_jul", "var_aug", "var_sep", "var_oct", "var_nov", "var_dec")
  nmss <- c("mean_winter", "mean_spring", "mean_summer", "mean_autumn")
  nvss <- c("var_winter", "var_spring", "var_summer", "var_autumn")
  numm <- c(
    "unit_mean_jan", "unit_mean_feb", "unit_mean_mar", "unit_mean_apr", 
    "unit_mean_may", "unit_mean_jun", "unit_mean_jul", "unit_mean_aug",
    "unit_mean_sep", "unit_mean_oct", "unit_mean_nov", "unit_mean_dec")
  nuvm <- c(
    "unit_var_jan", "unit_var_feb", "unit_var_mar", "unit_var_apr", 
    "unit_var_may", "unit_var_jun", "unit_var_jul", "unit_var_aug",
    "unit_var_sep", "unit_var_oct", "unit_var_nov", "unit_var_dec")
  nuwmm <- c(
    "unit_wmean_jan", "unit_wmean_feb", "unit_wmean_mar", "unit_wmean_apr", 
    "unit_wmean_may", "unit_wmean_jun", "unit_wmean_jul", "unit_wmean_aug",
    "unit_wmean_sep", "unit_wmean_oct", "unit_wmean_nov", "unit_wmean_dec")
  nuwvm <- c(
    "unit_wvar_jan", "unit_wvar_feb", "unit_wvar_mar", "unit_wvar_apr", 
    "unit_wvar_may", "unit_wvar_jun", "unit_wvar_jul", "unit_wvar_aug",
    "unit_wvar_sep", "unit_wvar_oct", "unit_wvar_nov", "unit_wvar_dec")
  numss <- c(
    "unit_mean_winter", "unit_mean_spring",
    "unit_mean_summer", "unit_mean_autumn")
  nuvss <- c(
    "unit_var_winter", "unit_var_spring", "unit_var_summer", "unit_var_autumn")
  # nint <- c("winter", "spring", "summer", "autumn")
  # Incorporation to output list
  sum_of_means <- sum(f$mean$monthly[,2])
  sum_of_extra_means <- sum(f$mean$seasons[,2])
  # Correction factor: as if all months had virtually 31 days
  corr_factor <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 31
  
  for (ii in 1:12) {
    o_f[[as.name(nmm[ii])]]  = f$mean$monthly[ii,2]
  }
  for (ii in 1:12) {
    o_f[[as.name(nvm[ii])]]  = f$var$monthly[ii,2]
  }
  for (ii in 1:12) {
    o_f[[as.name(numm[ii])]] = f$mean$monthly[ii,2] %/% sum_of_means
  }
  for (ii in 1:12) {
    o_f[[as.name(nuvm[ii])]] = f$var$monthly[ii,2] %/% sum_of_means
  }
  
  sum_of_wmeans <- 0
  for (ii in 1:12) {
    o_f[[as.name(nuwmm[ii])]] = o_f[[as.name(numm[ii])]] / corr_factor[ii]
    o_f[[as.name(nuwvm[ii])]] = o_f[[as.name(nuvm[ii])]] / corr_factor[ii]
    sum_of_wmeans <- sum_of_wmeans + o_f[[as.name(nuwmm[ii])]]
  }
  for (ii in 1:12) {
    o_f[[as.name(nuwmm[ii])]] = o_f[[as.name(nuwmm[ii])]] %/% sum_of_wmeans
    o_f[[as.name(nuwvm[ii])]] = o_f[[as.name(nuwvm[ii])]] %/% sum_of_wmeans
  }
  
  for (ii in 1:4) {
    o_f[[as.name(nmss[ii])]]  = f$mean$seasons[ii,2]
  }
  for (ii in 1:4) {
    o_f[[as.name(nvss[ii])]]  = f$var$seasons[ii,2]
  }
  for (ii in 1:4) {
    o_f[[as.name(numss[ii])]] = f$mean$seasons[ii,2] %/% sum_of_extra_means
  }
  for (ii in 1:4) {
    o_f[[as.name(nuvss[ii])]] = f$var$seasons[ii,2] %/% sum_of_extra_means
  }
  
  # for (ii in 4:1) {
  #   if (ii != 1) {
  #     for (jj in (ii-1):1) {
  #       rat_name <- as.name(paste("ratio_", nint[ii], "_to_", nint[jj], sep=""))
  #       top_name <- as.name(paste("mean_", nint[ii], sep=""))
  #       bot_name <- as.name(paste("mean_", nint[jj], sep=""))
  #       o_f[[rat_name]] <- o_f[[top_name]] / o_f[[bot_name]]
  #     }
  #   }
  # }

  return(o_f)
}

################################################################################
# get_bin_factors
################################################################################

#' Bin factors from a date sequence
#' 
#' @description 
#' Get the bin factors from a date sequence for all possible types.
#' 
#' @param t Date sequence.
#' @param type Type of the bins required: 1 hour, 4 hours, 6 hours, days, weekdays/weekends, months and Northern hemisphere meteorological seasons.
#' 
#' @return Factor of bins. 
#' 
#' @export

get_bin_factors <- function(t, type) {
  # By hours
  if (type == 1) {
    t_factor <- cut(t, breaks = "1 hour")
  }
  # By groups of 4 hours
  if (type == 2) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(hour(t_v) %% 4)
    t_factor <- as.factor(t_factor)
  }
  # By groups of 6 hours
  if (type == 3) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(hour(t_v) %% 6)
    t_factor <- as.factor(t_factor)
  }
  # By days
  if (type == 4) {
    t_factor <- cut(t, breaks = "1 day")
  }
  # By weekdays/weekends
  if (type == 5) {
    t_f <- cut(t, breaks = "1 day")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Numbers indicating 0-4 weekdays, 5-6 weekends
    t_factor <- (wday(t_v) - 2) %% 7
    # Sequence 0,1,2,3,4,0,1
    t_factor <- replace(t_factor, t_factor == 5, 0)
    t_factor <- replace(t_factor, t_factor == 6, 1)
    # Subtract properly to get the new groups
    t_factor <- t_v - days(t_factor)
    t_factor <- as.factor(t_factor)
  }
  # By months
  if (type == 6) {
    t_factor <- cut(t, breaks = "1 month")
  }
  # By Northern hemisphere meteorological seasons
  if (type == 7) {
    t_f <- cut(t, breaks = "1 month")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor <- t_v - months(month(t_v) %% 3)
    t_factor <- as.factor(t_factor)
  }
  return(t_factor)
}

################################################################################
# get_seasonal_features_from_timeseries
################################################################################

#' Seasonal features from time series
#' 
#' @description 
#' Get the mean values and standard deviations for all hours in a day, weekdays in a week and months in a year across a time series of class \code{msts}.
#' 
#' @param tseries Time series of class \code{msts}.
#' 
#' @return List of lists with the mean values and standard deviations.
#' 
#' @export

get_seasonal_features_from_timeseries <- function(tseries) {
  # Initialize results list
  o <- list()
  # Initial date
  ini_date <- get_extrema_dates_from_timeseries(tseries)
  # Date sequence
  samples_per_day <- attr(tseries, "msts")[1]
  date_by <- as.difftime(24 / samples_per_day, units = "hours")
  t <- seq(from = ini_date, length.out = length(tseries), by = date_by)
  # Variable names
  name <- c(
    as.name("hour_1"),
    as.name("hour_4"), 
    as.name("hour_6"),
    as.name("day"),
    as.name("weekday"),
    as.name("month"),
    as.name("season")
  )
  # Loop for the 7 different bins
  for (bb in 1:7) {
    # Get the bins to compute the sum
    sum_factor <- get_bin_factors(t, bb)
    # Aggregate data (sum) according to the bins
    aggr_data <- stats::aggregate(
      x   = as.numeric(tseries),
      by  = list(date_time = sum_factor),
      FUN = sum
    )
    # There's no need to check the completeness of the bins. Just remove first
    # and last bins by default. In case there's a unique bin when computing
    # means, standard deviation is set to 0. This may only happen with
    # meteorological seasons
    aggr_data <- aggr_data[c(-1, -dim(aggr_data)[1]), ]
    # Get the new bins to compute the mean
    # Hours
    if (bb == 1 | bb == 2 | bb == 3) {
      mean_factor <- as.factor(hour(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Days
    if (bb == 4 | bb == 5) {
      mean_factor <- as.factor(wday(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Months
    if (bb == 6 | bb == 7) {
      mean_factor <- as.factor(month(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Aggregate data (mean) according to the bins
    o[[name[[bb]]]]$"mean" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = mean_factor),
      FUN = mean
    )
    # Aggregate data (sd) according to the bins
    o[[name[[bb]]]]$"sd" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = mean_factor),
      FUN = sd
    )
  }
  return(o)
}