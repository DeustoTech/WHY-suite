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
    as.name("load_factor_mean_day"),
    as.name("load_factor_mean_week"),
    as.name("load_factor_mean_month")
  )
  sd_name_list <- c(
    as.name("load_factor_sd_day"),
    as.name("load_factor_sd_week"),
    as.name("load_factor_sd_month")
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
    load_factor[[  sd_name_list[[ii]]]] <- sd(avg_over_max)
  }
  return(load_factor)
}

################################################################################
# get_bins
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

get_bins <- function(t, type) {
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
    t_factor <- t_v - hours(lubridate::hour(t_v) %% 4)
    t_factor <- as.factor(t_factor)
  }
  # By groups of 6 hours
  if (type == 3) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(lubridate::hour(t_v) %% 6)
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
  # By groups of 4 hours and Northern hemisphere meteorological seasons
  if (type == 8) {
    ### 4-hour groups
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor_1 <- t_v - hours(lubridate::hour(t_v) %% 4)
    ### Season groups
    t_f <- cut(t, breaks = "1 month")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="GMT")
    # Subtract properly to get the new groups
    t_factor_2 <- t_v - months(month(t_v) %% 3)
    ### Combination
    t_factor <- ISOdate(
      year  = 2000,
      month = month(t_factor_2),
      day   = day(t_factor_2),
      hour  = hour(t_factor_1),
      tz    = "GMT"
    )
    t_factor <- as.factor(t_factor)
    browser()
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
#' @param maxmin Compute, in addition to mean and sd, max and min.
#' 
#' @return List of lists with the mean values and standard deviations.
#' 
#' @export

get_seasonal_features_from_timeseries <- function(tseries, maxmin = FALSE) {
  # Initialize results list
  o <- list()
  # DEFINITION OF FUNCTION TO AVOID NaN WHEN VECTOR LENGTH IS 1
  sd_ <- function(x) ifelse(length(x)==1, 0, stats::sd(x))
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
  # Loop for the 8 different bins
  for (bb in 1:8) {
    # Get the bins to compute the sum
    sum_factor <- get_bins(t, bb)
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
      sum_factor <- 
        as.factor(lubridate::hour(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Days
    if (bb == 4 | bb == 5) {
      sum_factor <- 
        as.factor(lubridate::wday(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Months
    if (bb == 6 | bb == 7) {
      sum_factor <- 
        as.factor(lubridate::month(as.POSIXct(aggr_data[,1], tz="GMT")))
    }
    # Aggregate data (mean) according to the bins
    o[[name[[bb]]]]$"mean" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = mean
    )
    # Aggregate data (sd) according to the bins
    o[[name[[bb]]]]$"sd" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = sd_
    )
    if (maxmin) {
      # Aggregate data (max) according to the bins
      o[[name[[bb]]]]$"max" <- stats::aggregate(
        x   = aggr_data$x,
        by  = list(bin = sum_factor),
        FUN = max
      )
      # Aggregate data (min) according to the bins
      o[[name[[bb]]]]$"min" <- stats::aggregate(
        x   = aggr_data$x,
        by  = list(bin = sum_factor),
        FUN = min
      )
    }
  }
  
  return(o)
}

################################################################################
# get_feature_names
################################################################################

#' Generation of the feature names
#' 
#' @description 
#' Generate the names of the features.
#' 
#' @details NAMING FORMAT: [abs|rel]_[mean|sd|max|mean]_type_(pday)
#' 
#' @return List of lists with the feature names.
#' 
#' @export

get_feature_names <- function() {
  n <- list()
  # 1-hour names
  n$str$hour1 <- c()
  for (ii in 0:23) {
    padded_num <- stringr::str_pad(ii, 2, pad = "0")
    n$str$hour1 <- c(n$str$hour1, paste(padded_num, "h", sep =""))
  }
  # 4-hour names
  n$str$hour4 <- c()
  for (ii in 0:5) {
    padded_num1 <- stringr::str_pad(ii*4, 2, pad = "0")
    padded_num2 <- stringr::str_pad(((ii + 1) * 4) %% 24, 2, pad = "0")
    n$str$hour4 <-
      c(n$str$hour4, paste(padded_num1, "h", padded_num2, "h", sep =""))
  }
  # 6-hour names
  n$str$hour6 <- c()
  for (ii in 0:3) {
    padded_num1 <- stringr::str_pad(ii*6, 2, pad = "0")
    padded_num2 <- stringr::str_pad(((ii + 1) * 6) %% 24, 2, pad = "0")
    n$str$hour6 <-
      c(n$str$hour6, paste(padded_num1, "h", padded_num2, "h", sep =""))
  }
  # Day names
  n$str$day <- c('sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat')
  # Weekday names
  n$str$weekday <- c('weekday', 'weekend')
  # Month names
  n$str$month <- c()
  for (ii in 1:12) {
    n$str$month <- c(n$str$month, tolower(month.abb[ii]))
  }
  # Season names
  n$str$season <- c('spring', 'summer', 'autumn', 'winter')
  
  ### Some constants
  # Days per weekday/weekend 
  n$const$dpw <- c(5, 2)
  # Days per month
  n$const$dpm <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  # Days per season
  n$const$dps <- c(90.25, 92, 92, 91)
  
  return(n)
}

################################################################################
# stat_data_aggregates
################################################################################

#' Features related to statistical data aggregates
#' 
#' @description 
#' Compute means and standard deviations of time series aggregates, where bins relate to day, week and year periods.
#' 
#' @details \code{RE-WRITE THIS} 
#' 
#' @param x Time series of class \code{msts}.
#'
#' @return A list with statistical values of the different bins.
#' @export

stat_data_aggregates <- function(x) {
  # Get the seasonal features
  f <- whyT2.1::get_seasonal_features_from_timeseries(x)
  # Output feature list
  o <- list()
  
  # DEFINITION OF FUNCTION TO AVOID NaN WHEN DIVIDING BY 0
  "%/%" <- function(x,y) ifelse(y==0, 0, x/y)
  
  # Get names & constants
  n <- get_feature_names()
  # Mean & sd strings
  msd_str <- c("mean", "sd", "max", "min")
  pd <- "pday"
  
  # Season loop
  for(ss in 1:7) {
    # Sum of means
    sum_of_means <- sum(f[[ss]]$mean$x)
    if (ss >= 5) {
      sum_of_wmeans <- sum(f[[ss]]$mean$x / n$const[[ss-4]])
    }
    # Loop mean|sd|max|min
    for (mm in 1:2) {
      # Loop for elements in the dataframe
      for (ii in 1:dim(f[[ss]]$mean)[1]) {
        # Assemble the name of the ABSOLUTE feature
        fname <- paste("abs", msd_str[mm], n$str[[ss]][ii], sep = "_")
        # Save the value of the feature
        o[[as.name(fname)]] <- f[[ss]][[mm]]$x[ii]
        # Assemble the name of the RELATIVE feature
        fname <- paste("rel", msd_str[mm], n$str[[ss]][ii], sep = "_")
        # Save the value of the feature
        o[[as.name(fname)]] <- f[[ss]][[mm]]$x[ii] %/% sum_of_means
        # For seasons higher than the day
        if (ss >= 5) {
          # Assemble the name of the ABSOLUTE feature
          fname <- paste("abs", msd_str[mm], n$str[[ss]][ii], pd, sep = "_")
          # Save the value of the feature
          o[[as.name(fname)]] <- f[[ss]][[mm]]$x[ii] %/% n$const[[ss-4]][ii]
          # Assemble the name of the RELATIVE feature
          fname <- paste("rel", msd_str[mm], n$str[[ss]][ii], pd, sep = "_")
          # Save the value of the feature
          o[[as.name(fname)]] <- 
            f[[ss]][[mm]]$x[ii] %/% n$const[[ss-4]][ii] %/% sum_of_wmeans
        }
      }
    }
  }
  
  return(o)
}