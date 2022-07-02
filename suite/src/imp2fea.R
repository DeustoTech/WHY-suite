################################################################################
# CARLOS QUESADA GRANJA
# 9 de febrero de 2022
# UNIVERSIDAD DE DEUSTO
# ---------------------
# Definitive file for generating a feature file from imputed folders
################################################################################

library(tsfeatures)
library(foreach)
library(lubridate)
library(dplyr)

################################################################################
# stat_moments
################################################################################

#' Features related to statistical moments
#'
#' @description
#' Compute the mean, variance, skewness and kurtosis of a time series. It also includes the sum of the entire time series.
#'
#' @param x Time series of class \code{msts}.
#'
#' @return A list with the mean, variance, skewness and kurtosis of the time series.
#'
#' @export

stat_moments <- function(x) {
  list(
    mean     = mean(x),
    variance = var(x),
    skewness = moments::skewness(x),
    kurtosis = moments::kurtosis(x),
    sum      = sum(x)
  )
}

################################################################################
# quantiles
################################################################################

#' Features related to quantiles
#'
#' @description
#' Compute the minimum, median and maximum, quartiles, deciles, interquartile (Q3 to Q1) range and percentage of outliers.
#'
#' @param x Time series of class \code{msts}.
#'
#' @return A list with the minimum, median and maximum, quartiles, deciles, interquartile (Q3 to Q1) range and percentage of outliers.
#'
#' @export

quantiles <- function(x) {
  # Compute quartiles
  q <- stats::quantile(x)
  # Compute deciles
  d <- stats::quantile(x, probs = seq(0, 1, 0.1))
  # Interquartile range
  iqr <- q[[4]] - q[[2]]
  # IQR criterion for outlier detection
  is_outlier <- x < q[[2]] - 1.5 * iqr | x > q[[4]] + 1.5 * iqr
  # Percentage of outliers 
  outlier_pc <- sum(is_outlier) / length(x)
  
  list(
    minimum        = q[[1]],
    maximum        = q[[5]],
    median         = q[[3]],
    quartile_1     = q[[2]],
    quartile_3     = q[[4]],
    
    decile_1       = d[[2]],
    decile_2       = d[[3]],
    decile_3       = d[[4]],
    decile_4       = d[[5]],
    decile_6       = d[[7]],
    decile_7       = d[[8]],
    decile_8       = d[[9]],
    decile_9       = d[[10]],
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
# daily_acf
################################################################################

#' Features related to the autocorrelation lags for each day
#'
#' @description
#' Compute the autocorrelation lags for each day, from day 1 to 28.
#'
#' @param x Time series of class \code{msts}.
#'
#' @return A list with the value of the autocorrelation for each day from 1 to
#' 28.
#' @export

daily_acf <- function(x) {
  # Samples per day
  samples_per_day <- attr(x, "msts")[1]
  # Autocorrelation function
  acfx <- acf(x, lag.max = 30 * samples_per_day, plot = F)
  # Loop
  ac_list <- list()
  for (ii in 1:28) {
    feat_name <- as.name(paste("ac_day", ii, sep="_"))
    ac_list[[feat_name]] <- acfx$acf[1 + samples_per_day * ii]
  }
  return(ac_list)
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
  library(lubridate)
  # By hours
  if (type == 1) {
    t_factor <- cut(t, breaks = "1 hour")
  }
  # By groups of 4 hours
  if (type %in% c(2,18)) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(hour(t_v) %% 4)
    t_factor <- as.factor(t_factor)
  }
  # By groups of 6 hours
  if (type == 3) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(hour(t_v) %% 6)
    t_factor <- as.factor(t_factor)
  }
  # By days
  if (type %in% c(4,15:17)) {
    t_factor <- cut(t, breaks = "1 day")
  }
  # By weekdays/weekends
  if (type == 5) {
    t_f <- cut(t, breaks = "1 day")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
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
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Subtract properly to get the new groups
    t_factor <- t_v - months(month(t_v) %% 3)
    t_factor <- as.factor(t_factor)
  }
  # By groups of 4 hours and Northern hemisphere meteorological seasons
  if (type == 8) {
    ### 4-hour groups
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Subtract properly to get the new groups
    t_factor_1 <- t_v - hours(hour(t_v) %% 4)
    ### Season groups
    t_f <- cut(t, breaks = "1 month")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Subtract properly to get the new groups
    t_factor_2 <- t_v - months(month(t_v) %% 3)
    ### Combination
    t_factor <- ISOdate(
      year  = year(t_factor_2),
      month = month(t_factor_2),
      day   = day(t_factor_2),
      hour  = hour(t_factor_1),
      tz    = "UTC"
    )
    t_factor <- as.factor(t_factor)
  }
  # By 2.0TD periods excluding weekends
  if (type %in% c(9, 11)) {
    t_f <- cut(t, breaks = "1 hour") 
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    # Numbers indicating hours
    t_factor_1 <- hour(t_v)
    # Subtraction sequence
    idx1 <- t_factor_1 %in% c(8,10,14,18,22)
    idx2 <- t_factor_1 %in% c(9,11,15,19,23)
    idx3 <- t_factor_1 %in% c(12,16,20)
    idx4 <- t_factor_1 %in% c(13,17,21)
    # Applying corrections
    t_factor <- t_factor_1
    t_factor[idx1] <- 0
    t_factor[idx2] <- 1
    t_factor[idx3] <- 2
    t_factor[idx4] <- 3
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(t_factor)
    t_factor <- as.factor(t_factor)
  }
  # By 2.0TD periods including weekends
  if (type %in% c(10, 12)) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    ### Workday groups
    # Numbers indicating hours
    t_factor_1 <- hour(t_v)
    t_factor_2 <- (wday(t_v) - 2) %% 7
    # Subtraction sequence
    idx1 <- t_factor_1 %in% c(8,10,14,18,22) & t_factor_2 %in% 0:4
    idx2 <- t_factor_1 %in% c(9,11,15,19,23) & t_factor_2 %in% 0:4
    idx3 <- t_factor_1 %in% c(12,16,20)      & t_factor_2 %in% 0:4
    idx4 <- t_factor_1 %in% c(13,17,21)      & t_factor_2 %in% 0:4
    idx5 <- t_factor_2 == 6
    # Applying corrections
    t_factor <- t_factor_1
    t_factor[idx1] <- 0
    t_factor[idx2] <- 1
    t_factor[idx3] <- 2
    t_factor[idx4] <- 3
    t_factor[idx5] <- t_factor[idx5] + 24
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(t_factor)
    t_factor <- as.factor(t_factor)
  }
  # By 2.0TD periods including weekends
  if (type %in% c(13, 14)) {
    t_f <- cut(t, breaks = "1 hour")
    # Convert to vector of dates
    t_v <- as.POSIXct(as.vector(t_f), tz="UTC")
    ### Workday groups
    # Numbers indicating hours
    t_factor_1 <- hour(t_v)
    t_factor_2 <- wday(t_v)
    # Subtraction sequence
    idx0  <- t_factor_1 %in% c(8,10)  & t_factor_2 %in% 0:4
    idx1  <- t_factor_1 %in% c(9,11)  & t_factor_2 %in% 0:4
    idx2  <- t_factor_1 %in% c(12)    & t_factor_2 %in% 0:4
    idx3  <- t_factor_1 %in% c(13)    & t_factor_2 %in% 0:4
    idx6  <- t_factor_1 %in% c(14)    & t_factor_2 %in% 0:4
    idx7  <- t_factor_1 %in% c(15)    & t_factor_2 %in% 0:4
    idx8  <- t_factor_1 %in% c(16,18) & t_factor_2 %in% 0:4
    idx9  <- t_factor_1 %in% c(17,19) & t_factor_2 %in% 0:4
    idx10 <- t_factor_1 %in% c(20)    & t_factor_2 %in% 0:4
    idx11 <- t_factor_1 %in% c(21)    & t_factor_2 %in% 0:4
    idx14 <- t_factor_1 %in% c(22)    & t_factor_2 %in% 0:4
    idx15 <- t_factor_1 %in% c(23)    & t_factor_2 %in% 0:4
    idxF2 <- t_factor_2 == 1
    # Applying corrections
    t_factor <- t_factor_1
    t_factor[idx0]  <- 0
    t_factor[idx1]  <- 1
    t_factor[idx2]  <- 2
    t_factor[idx3]  <- 3
    t_factor[idx6]  <- 6
    t_factor[idx7]  <- 7
    t_factor[idx8]  <- 8
    t_factor[idx9]  <- 9
    t_factor[idx10] <- 10
    t_factor[idx11] <- 11
    t_factor[idx14] <- 14
    t_factor[idx15] <- 15
    t_factor[idxF2] <- t_factor[idxF2] + 24
    # Subtract properly to get the new groups
    t_factor <- t_v - hours(t_factor)
    t_factor <- as.factor(t_factor)
  }
  return(t_factor)
}

################################################################################
# catch22_features
################################################################################
#' Catch-22 features from time series
#' 
#' @description 
#' Get the Catch-22 features from a time series of class \code{msts}.
#' 
#' @param tseries Time series of class \code{msts}.
#' 
#' @return List of Catch-22 features.
#' 
#' @export

catch22_features <- function(x) {
  # Compute Catch-22 features
  catch22_feats <- catch22::catch22_all(x)
  # Return
  o <- catch22_feats$values
  names(o) <- catch22_feats$names
  return(o)
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
    as.name("hour_1"),           #  1
    as.name("hour_4"),           #  2
    as.name("hour_6"),           #  3
    as.name("day"),              #  4
    as.name("weekday"),          #  5
    as.name("month"),            #  6
    as.name("season"),           #  7
    as.name("hour4_season"),     #  8
    as.name("td2.0_p6"),         #  9 
    as.name("td2.0_p7"),         # 10
    as.name("td2.0_p6_season"),  # 11
    as.name("td2.0_p7_season"),  # 12
    as.name("td2.0_p3"),         # 13
    as.name("td2.0_p3_ym"),      # 14 
    as.name("weekday_drm"),      # 15 >> drm = day-referenced mean
    as.name("month_drm"),        # 16 
    as.name("season_drm"),       # 17
    as.name("hour4_season_drm")  # 18
  )
  # Loop for the 8 different bins
  for (bb in 1:18) {
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
    if (bb %in% 1:8) {
      aggr_data <- aggr_data[c(-1, -nrow(aggr_data)), ]
    }
    # Get the new bins to compute the mean
    # Hours
    if (bb == 1 | bb == 2 | bb == 3) {
      sum_factor <- 
        as.factor(lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Days
    if (bb == 4 | bb == 5) {
      sum_factor <- 
        as.factor(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Months
    if (bb == 6 | bb == 7) {
      sum_factor <- 
        as.factor(lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Hours & seasons
    if (bb == 8) {
      sum_factor <- as.factor(
        lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")) + 
          lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) * 100
      )
    }
    # 2.0TD with 6 periods
    if (bb == 9) {
      sum_factor <- 
        as.factor(lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # 2.0TD with 7 periods
    if (bb == 10) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 25
      sum_factor <- as.factor(sum_factor)
    }
    # 2.0TD with 6 periods per season (24 bins)
    if (bb == 11) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Season detection
      idx <- (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * idx + sum_factor)
    }
    # 2.0TD with 7 periods per season (28 bins)
    if (bb == 12) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 25
      idx <- (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * idx + sum_factor)
    }
    # 2.0TD with 6 periods
    if (bb == 13) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor[sum_factor ==  0] <- 3
      sum_factor[sum_factor ==  8] <- 2
      sum_factor[sum_factor == 10] <- 1
      sum_factor <- as.factor(sum_factor)
    }
    # 2.0TD year x month x period (28 bins)
    if (bb == 14) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection (P3)
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 3
      sum_factor[sum_factor == 0] <- 3
      # P2
      sum_factor[sum_factor %in% c(8, 14, 22)] <- 2
      # P1 
      sum_factor[sum_factor %in% c(10, 18)] <- 1
      idx <- lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor <- 10 * idx + sum_factor
      idx <- lubridate::year(as.POSIXct(aggr_data[,1], tz="UTC")) - 2000
      sum_factor <- as.factor(1000 * idx + sum_factor)
    }
    # Day-referenced mean: weekends/workdays 
    if (bb == 15) {
      sum_factor <- lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor[sum_factor %in% c(1,7)] <- 7
      sum_factor[sum_factor %in% c(2:6)] <- 1
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: month 
    if (bb == 16) {
      sum_factor <- lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: season 
    if (bb == 17) {
      sum_factor <-
        (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: 4-hour period & season
    if (bb == 18) {
      sum_factor_1 <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor_2 <- 
        (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * sum_factor_2 + sum_factor_1)
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
    # Aggregate data (sum) according to the bins
    o[[name[[bb]]]]$"sum" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = sum
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
#' @details NAMING FORMAT: [abs|rel]_[mean|sd|max|mean|sum]_type_(pday)
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
  # 4-hour & seasons names
  n$str$hour4_season <- c()
  for (ii in 0:23) {
    # Hours
    jj <- ii %% 6
    padded_num1 <- stringr::str_pad(jj*4, 2, pad = "0")
    padded_num2 <- stringr::str_pad(((jj + 1) * 4) %% 24, 2, pad = "0")
    x <- paste(padded_num1, "h", padded_num2, "h", sep ="")
    # Seasons
    kk <- floor(ii / 6) + 1
    x <- paste(x, substr(n$str$season[[kk]], 1, 3), sep="")
    n$str$hour4_season <- c(n$str$hour4_season, x)
  }
  
  # as.name("td2.0_p6"),         #  9 
  # as.name("td2.0_p7"),         # 10
  # as.name("td2.0_p6_season"),  # 11
  # as.name("td2.0_p7_season"),  # 12
  # as.name("td2.0_p3"),         # 13
  # as.name("td2.0_p3_ym"),      # 14 
  # as.name("weekday_drm"),      # 15 >> drm = day-referenced mean
  # as.name("month_drm"),        # 16 
  # as.name("season_drm"),       # 17
  # as.name("hour4_season_drm")  # 18
  
  wperiods <- c("00h08h", "08h10h", "10h14h", "14h18h", "18h22h", "22h00h")
  n$str$td2.0_p6 <- paste0("td2.0_p6_", wperiods)
  n$str$td2.0_p7 <- paste0("td2.0_p7_", c(wperiods, "wkends"))
  n$str$td2.0_p6_season <- paste(
    rep(n$str$td2.0_p6, times=4),
    rep(substr(n$str$season[c(4,1:3)],1,3), each=6),
    sep="_")
  n$str$td2.0_p7_season <- paste(
    rep(n$str$td2.0_p7, times=4),
    rep(substr(n$str$season[c(4,1:3)],1,3), each=7),
    sep="_")
  n$str$td2.0_p3 <- paste0("td2.0_p3_", c("punta", "llano", "valle"))
  
  wmonths <- 1:12
  wyears <- 14:22
  n$str$td2.0_p3_ym <- paste(
    "td2.0_pmy",
    rep(c("punta", "llano", "valle"), length(wyears)*length(wmonths)),
    rep(rep(sprintf("%02d", wmonths), each=3), length(wyears)),
    rep(sprintf("%02d", wyears), each=3*length(wmonths)),
    sep="_"
  )
  
  # td2.0_p3_ym <- function(wperiod, wmonth, wyear) {
  #   p3 <- c("punta", "llano", "valle")
  #   paste(
  #     "td2.0_pmy",
  #     p3[wperiod],
  #     sprintf("%02d", wmonth),
  #     wyear,
  #     sep="_"
  #   )
  # }
  
  n$str$weekday_drm <- paste0(n$str$weekday, "_drm")
  n$str$month_drm <- paste0(n$str$month, "_drm")
  n$str$season_drm <- paste0(n$str$season[c(4,1:3)], "_drm")
  n$str$hour4_season_drm <- paste0(n$str$hour4_season[c(19:24,1:18)], "_drm")
  
  ### Some constants
  # Days per weekday/weekend 
  n$const$dpw <- c("2"=5, "7"=2)
  # Days per month
  n$const$dpm <- c("1"=31, "2"=28.25, "3"=31, "4"=30, "5"=31, "6"=30,
                   "7"=31, "8"=31, "9"=30, "10"=31, "11"=30, "12"=31)
  # Days per season
  n$const$dps <- c("3"=92, "6"=92, "9"=91, "12"=90.25)
  # Days per hour & season
  n$const$dphs <- rep(n$const$dps, each=6)
  names(n$const$dphs) <- c(
    "300", "304", "308", "312", "316", "320",
    "600", "604", "608", "612", "616", "620",
    "900", "904", "908", "912", "916", "920",
    "1200", "1204", "1208", "1212", "1216", "1220")
  return(n)
}

################################################################################
# get_peak_times
################################################################################

#' Peak and off-peak times
#' 
#' @description 
#' Indicates the peak and off-peaks times of all the available seasons
#' 
#' @details Possible values. hour_1: 0 to 23; hour_4: 0 (from 00:00 to 04:00), 4, 8, 12, 16, 20; hour_6: 0 (from 00:00 to 06:00), 6, 12, 18; day: from 1 (Sunday) to 7 (Saturday); weekday: 2 (weekdays) or 7 (weekends); month: 1 to 12; season: 3 (spring), 6 (summer), 9 (autumn), 12 (winter); hour4_season: two last digits indicate hour_4, remaining digits indicate season.
#' 
#' @return List with feature values of peaks and off-peaks.
#' 
#' @export

get_peak_times <- function(ft) {
  # Initialize output
  o <- list()
  
  # Some constants
  const <- list()
  # Days per weekday/weekend 
  const$dpw <- c("2"=5, "7"=2)
  # Days per month
  const$dpm <- c("1"=31, "2"=28.25, "3"=31, "4"=30, "5"=31, "6"=30,
                 "7"=31, "8"=31, "9"=30, "10"=31, "11"=30, "12"=31)
  # Days per season
  const$dps <- c("3"=92, "6"=92, "9"=91, "12"=90.25)
  # Days per hour & season
  const$dphs <- rep(const$dps, each=6)
  names(const$dphs) <- c(
    "300", "304", "308", "312", "316", "320",
    "600", "604", "608", "612", "616", "620",
    "900", "904", "908", "912", "916", "920",
    "1200", "1204", "1208", "1212", "1216", "1220")
  
  ### Loop of types
  for (ii in 1:8) {
    # Peak
    time_name <- paste("peak", names(ft)[ii], sep="_")
    idx <- which.max(ft[[ii]][[1]]$x)
    o[[as.name(time_name)]] <- as.numeric(levels(ft[[ii]][[1]]$bin)[idx])
    # Off-peak
    time_name <- paste("off_peak", names(ft)[ii], sep="_")
    idx <- which.min(ft[[ii]][[1]]$x)
    o[[as.name(time_name)]] <- as.numeric(levels(ft[[ii]][[1]]$bin)[idx])
  }
  
  ### Loop of types per day
  for (ii in 5:8) {
    # Peak
    time_name <- paste("peak", names(ft)[ii], "pday", sep="_")
    idx <- which.max(ft[[ii]][[1]]$x / const[[ii-4]][as.character(ft[[ii]]$mean$bin)])
    o[[as.name(time_name)]] <- as.numeric(levels(ft[[ii]][[1]]$bin)[idx])
    # Off-peak
    time_name <- paste("off_peak", names(ft)[ii], "pday", sep="_")
    idx <- which.min(ft[[ii]][[1]]$x / const[[ii-4]][as.character(ft[[ii]]$mean$bin)])
    o[[as.name(time_name)]] <- as.numeric(levels(ft[[ii]][[1]]$bin)[idx])
  }
  
  return(o)
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

get_expected_bins <- function() {
  # Getting the most complex bins
  num_11 <- c()
  for (ii in 1:4) {
    num_11 <- c(num_11, 100*ii+c(0,8,10,14,18,22))
  }
  num_12 <- c()
  for (ii in 1:4) {
    num_12 <- c(num_12, 100*ii+c(0,8,10,14,18,22,25))
  }
  num_14 <- c()
  for (ii in 14:22) {
    for (jj in 1:12) {
      for (kk in 1:3) {
        num_14 <- c(num_14, ii*1000 + jj*10 + kk)   
      }
    }
  }
  # Return
  return(
    list(
      # as.name("hour_1"),           #  1
      0:23,
      # as.name("hour_4"),           #  2
      seq(0,20,4),
      # as.name("hour_6"),           #  3
      seq(0,18,6),
      # as.name("day"),              #  4
      1:7,
      # as.name("weekday"),          #  5
      c(2,7),
      # as.name("month"),            #  6
      1:12,
      # as.name("season"),           #  7
      seq(3,12,3),
      # as.name("hour4_season"),     #  8
      c(seq(300,320,4), seq(600,620,4), seq(900,920,4), seq(1200,1220,4)),
      # as.name("td2.0_p6"),         #  9 
      c(0,8,10,14,18,22),
      # as.name("td2.0_p7"),         # 10
      c(0,8,10,14,18,22,25),
      # as.name("td2.0_p6_season"),  # 11
      num_11,
      # as.name("td2.0_p7_season"),  # 12
      num_12,
      # as.name("td2.0_p3"),         # 13
      1:3,
      # as.name("td2.0_p3_ym"),      # 14
      num_14,
      # as.name("weekday_drm"),      # 15 >> drm = day-referenced mean
      c(1,7),
      # as.name("month_drm"),        # 16
      1:12,
      # as.name("season_drm"),       # 17
      1:4,
      # as.name("hour4_season_drm")  # 18
      c(seq(100,120,4), seq(200,220,4), seq(300,320,4), seq(400,420,4))
    )
  )
}


stat_data_aggregates <- function(x) {
  # Get the seasonal features
  ft <- get_seasonal_features_from_timeseries(x)
  # Get peak and off-peak times
  o <- get_peak_times(ft)
  #o <- list()
  
  # Absolute or relative
  abs_rel_str <- c("abs", "rel")
  # Operations
  operations_str <- c("mean", "sd", "sum", "max", "min")
  
  fnames <- get_feature_names()
  expected_bins <- get_expected_bins()
  
  # DEFINITION OF FUNCTION TO AVOID NaN WHEN DIVIDING BY 0
  "%/%" <- function(x,y) ifelse(y==0, 0, x/y)
  
  for(ss in 9:18) {
    # Sum of means
    sum_of_means <- sum(ft[[ss]]$mean$x)
    # Sum of sums
    sum_of_sums <- sum(ft[[ss]]$sum$x)
    
    # Operations loop (mean, sd, sum, max, min)
    for (mm in 1:3) {
      # Absolute or relative
      for (ar in 1:2) {
        # Loop for elements in the list of expected_bins
        for (bb in 1:length(expected_bins[[ss]])) {
          # Get current bin
          current_bin <- expected_bins[[ss]][bb]
          # Get name associated to current bin
          current_fname <- paste(
            abs_rel_str[ar],
            operations_str[mm],
            fnames[[1]][[ss]][bb],
            sep = "_"
          )
          # Get value associated to the feature name
          current_value <- ft[[ss]][[mm]]$x[ft[[ss]][[mm]]$bin==current_bin]
          if (length(current_value) == 0) current_value <- NA
          
          # Save the value of the feature
          if (ar == 1) {
            # Absolute value
            o[[as.name(current_fname)]] <- current_value
          } else {
            # Relative value
            o[[as.name(current_fname)]] <- current_value %/%
              ifelse(mm == 3, sum_of_sums, sum_of_means)
          }
        }
      }
    }
  }
  
  for(ss in 1:8) {
    # Sum of means
    sum_of_means <- sum(ft[[ss]]$mean$x)
    if (ss >= 5) {
      sum_of_wmeans <- sum(
        ft[[ss]]$mean$x / fnames$const[[ss-4]][as.character(ft[[ss]]$mean$bin)]
      )
    }
    
    # Operations loop (mean, sd, sum, max, min)
    for (mm in 1:2) {
      # Absolute or relative
      for (ar in 1:2) {
        # Loop for elements in the list of expected_bins
        for (bb in 1:length(expected_bins[[ss]])) {
          # Get current bin
          current_bin <- expected_bins[[ss]][bb]
          # Get name associated to current bin
          current_fname <- paste(
            abs_rel_str[ar],
            operations_str[mm],
            fnames[[1]][[ss]][bb],
            sep = "_"
          )
          # Get value associated to the feature name
          current_value <- ft[[ss]][[mm]]$x[ft[[ss]][[mm]]$bin==current_bin]
          if (length(current_value) == 0) current_value <- NA
          
          # Save the value of the feature
          if (ar == 1) {
            # Absolute value
            o[[as.name(current_fname)]] <- current_value
            
            if (ss >= 5) {
              # Assemble the name of the RELATIVE PER DAY feature
              current_fname <- paste0(current_fname, "_pday")
              # Save the value of the feature
              o[[as.name(current_fname)]] <-
                current_value %/% fnames$const[[ss-4]][bb]
              # Remove name
              names(o[[as.name(current_fname)]]) <- NULL
            }
            
          } else {
            # Relative value
            o[[as.name(current_fname)]] <- current_value %/%
              ifelse(mm == 3, sum_of_sums, sum_of_means)
            
            if (ss >= 5) {
              # Assemble the name of the RELATIVE PER DAY feature
              current_fname <- paste0(current_fname, "_pday")
              # Save the value of the feature
              o[[as.name(current_fname)]] <-
                current_value %/% fnames$const[[ss-4]][bb] %/% sum_of_wmeans
              # Remove name
              names(o[[as.name(current_fname)]]) <- NULL
            }
          }
        }
      }
    }
  }
  
  return(o)
}

################################################################################
# get_timeseries_from_cooked_dataframe
################################################################################

#' Time series from cooked dataframe
#' 
#' @description 
#' Creates a multi-seasonal time series \code{msts} from a coocked dataframe (or superior).
#' 
#' @param df Cooked dataframe.
#' 
#' @return Multi-seasonal time series.
#'
#' @export

get_timeseries_from_cooked_dataframe <- function(cdf) {
  # Compute "start" parameters used in "msts"
  initial_date  <- cdf$df[1,1]
  lowest_season <- cdf$seasonal_periods[1]
  hour_factor   <- lowest_season / 24
  # Number of days since 1970-01-01
  ref_day       <- as.Date("1970-01-01")
  start_yearday <- lubridate::day(lubridate::days(
    as.Date(initial_date) - ref_day))
  start_offset  <- 1 + lubridate::hour(initial_date) * hour_factor +
    lubridate::minute(initial_date) / 60 * hour_factor + 
    lubridate::second(initial_date) / 3600 * hour_factor
  
  # Convert to time series (using "msts")
  tseries <- forecast::msts(
    data             = cdf$df[,2],
    seasonal.periods = unique(cdf$seasonal_periods),
    ts.frequency     = lowest_season, 
    start            = c(start_yearday, start_offset)
  )
  
  return(tseries)
}

################################################################################
# get_extrema_dates_from_timeseries
################################################################################

#' Initial and final dates from time series
#' 
#' @description 
#' Get initial and final dates from a time series. If parameter \code{only_initial} is set to \code{TRUE}, return only initial date.
#' 
#' @param tseries Time series of class \code{msts}.
#' @param only_initial If \code{TRUE}, returns only initial date.
#' 
#' @return List with initial and final dates or initial date if \code{only_initial} is set to \code{TRUE}.
#' 
#' @export

get_extrema_dates_from_timeseries <- function(tseries, only_initial=TRUE) {
  # First seasonal period
  sp <- attr(tseries, "msts")[1]
  # Initial date
  initial_date <- as.Date("1970-01-01") + start(tseries)[1]
  # Initial time
  td <- lubridate::seconds_to_period((start(tseries)[2] - 1) / sp * 86400)
  initial_time <- sprintf("%02d:%02d:%02d", 
                          lubridate::hour(td),
                          lubridate::minute(td),
                          lubridate::second(td))
  # Initial time-date
  initial_td <- as.POSIXct(paste(initial_date, initial_time), tz="UTC")
  
  # In case "only_initial" is TRUE
  if (only_initial) {
    
    return(initial_td)
    
  } else {
    # Final date
    final_date <- as.Date("1970-01-01") + end(tseries)[1]
    # Final time
    td <- lubridate::seconds_to_period((end(tseries)[2] - 1) / sp * 86400)
    final_time <- sprintf("%02d:%02d:%02d", 
                          lubridate::hour(td),
                          lubridate::minute(td),
                          lubridate::second(td))
    # Final time-date
    final_td <- as.POSIXct(paste(final_date, final_date), tz="UTC")
    
    return(list(initial_date = initial_td, final_date = final_td))
  }
}

################################################################################
# get_features_from_cooked_dataframe
################################################################################

#' Features of a cooked dataframe
#'
#' @description
#' Get features of a cooked (or extended) dataframe.
#'
#' @param df Cooked (or extended) dataframe.
#' @param type_of_analysis A string indicating the type of analysis: \code{basic}, \code{extra} or \code{custom}.
#' @param list_of_functions If \code{type_of_analysis} is \code{custom}, a list of strings indicating the functions that perform the feature extraction.
#' @param If \code{type_of_analysis} is \code{custom}, TRUE or FALSE indicating if the time series must be scaled to mean 0 and sd 1 prior the analysis.
#'
#' @return List of features.
#'
#' @export

get_features_from_cooked_dataframe <- function(cdf, type_of_analysis, list_of_functions=c(), .scale=FALSE) {
  # Set a seed for random numbers
  set.seed(1981)
  # Get multiseasonal time series
  tseries <- get_timeseries_from_cooked_dataframe(cdf)
  
  ### type_of_analysis is CUSTOM
  if (type_of_analysis == "custom") {
    feats <- tsfeatures::tsfeatures(
      tslist    = list(tseries),
      features  = list_of_functions,
      scale     = .scale,    # <-- time series ARE SCALED to mean 0 and sd 1
      na.action = forecast::na.interp
    )
  }
  ### type_of_analysis is BASIC or EXTRA
  else {
    # List of functions that DON'T require normalization -> they are included in 
    # BOTH "basic" and "extra" analyses
    not_norm_fns <- c(
      "stat_moments", "quantiles", "stat_data_aggregates") #, "load_factors")
    # List of BASIC functions that REQUIRE normalization
    basic_fns <- c(
      "frequency", "stl_features", "entropy", "acf_features", "daily_acf")#, "catch22_features")
    # List of EXTRA functions that REQUIRE normalization
    extra_fns <- c(
      "max_kl_shift", "outlierinclude_mdrmd", "arch_stat", 
      "max_level_shift", "ac_9", "crossing_points", "max_var_shift",
      "nonlinearity", "spreadrandomlocal_meantaul", "flat_spots",
      "pacf_features","firstmin_ac", "std1st_der", "heterogeneity", "stability", 
      "firstzero_ac", "trev_num", "holt_parameters", "walker_propcross", 
      "hurst", "unitroot_kpss", "histogram_mode", "unitroot_pp",
      "localsimple_taures", "lumpiness", "motiftwo_entro3")
    # List of functions that REQUIRE normalization ("extra" includes "basic")
    analysis_fns <- list(
      basic = basic_fns,
      extra = c(basic_fns, extra_fns)
    )
    
    # Extract features that DON'T require normalization of the time series
    not_norm_feats <- tsfeatures::tsfeatures(
      tslist    = list(tseries),
      features  = not_norm_fns,
      scale     = FALSE,   # <-- time series are NOT SCALED
      na.action = forecast::na.interp
    )
    # Extract features that REQUIRE normalization of the time series
    norm_feats <- tsfeatures::tsfeatures(
      tslist    = list(tseries),
      features  = analysis_fns[[type_of_analysis]],
      scale     = TRUE,    # <-- time series ARE SCALED to mean 0 and sd 1
      na.action = forecast::na.interp
    )
    # Bind features into a unique dataframe
    feats <- cbind(not_norm_feats, norm_feats)
  }
  return(feats)
}

################################################################################
# get_features_from_raw_datasets
################################################################################

#' Features of raw datasets in a folder
#'
#' @description
#' Get features of all datasets contained in a folder.
#'
#' @param folder_path String with the absolute path to the dataset folder (ending in `/`).
#' @param from_date Initial date and time of the interval. Either a \code{POSIXct} class in the UTC time zone OR the string \code{first}.
#' @param to_date Final date and time of the interval. Either a \code{POSIXct} class in the UTC time zone OR the string \code{last}. 
#' @param dset_key Key of the dataset.
#' @param allowed_na A numerical value between 0 and 1. It represents the maximum percentage of admissible \code{NA} values in the cooked dataframe for which the feature extraction is performed. The \code{NA} values will be STL-interpolated (using \code{forecast::na.interp}) prior to the feature extraction.
#' @param type_of_analysis A string indicating the type of analysis: either \code{basic} or \code{extra}.
#' @param output_folder_path String with the absolute path to the output folder (ending in `/`).
#'
#' @return List of dataframes of (1) extracted features and (2) accepted and (3) rejected files for feature extraction.
#'
#' @export

get_features_from_raw_datasets <- function(folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis, output_folder_path=NULL) {
  # Initialization of outputs
  features <- NULL
  accepted <- NULL
  rejected <- NULL
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(folder_path)
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset
    file_path <- paste(folder_path, dset_filename, sep="")
    raw_df    <- get_raw_dataframe_from_dataset(file_path)
    # Get cooked dataframe from raw dataframe
    cooked_df <- cook_raw_dataframe(raw_df, from_date, to_date, dset_key)
    
    # Accept to extract features
    na_percentage <- cooked_df$number_of_na/dim(cooked_df$df)[1]
    if (na_percentage <= allowed_na & !cooked_df$is_0) {
      # GET FEATURES
      ff <- get_features_from_cooked_dataframe(cooked_df, type_of_analysis)
      # Incorporate features to output
      features <- rbind(features, ff)
      # Create dataframe for accepted dataset
      aa <- data.frame(
        filename      = dset_filename,
        from_date     = format(from_date, "%Y-%m-%d %H:%M:%S"),
        to_date       = format(to_date, "%Y-%m-%d %H:%M:%S"),
        samples       = dim(cooked_df$df)[1],
        na_percentage = na_percentage
      )
      # Incorporate accepted dataframe to output
      accepted <- rbind(accepted, aa)
      print("Features extracted!")
    }
    
    # Reject to extract features
    else {
      # Create dataframe for rejected dataset
      rr <- data.frame(
        filename      = dset_filename,
        from_date     = format(from_date, "%Y-%m-%d %H:%M:%S"),
        to_date       = format(to_date, "%Y-%m-%d %H:%M:%S"),
        na_percentage = na_percentage,
        is_0          = cooked_df$is_0
      )
      # Incorporate rejected dataframe to output
      rejected <- rbind(rejected, rr)
      print("Features not extracted")
    }
  }
  
  # Save dataframes as CSV
  utils::write.table(
    features,
    file = paste(output_folder_path, "features.csv", sep=""),
    row.names = FALSE,
    sep = ",",
    na = "",
    quote = FALSE
  )
  utils::write.table(
    accepted,
    file = paste(output_folder_path, "accepted.csv", sep=""),
    row.names = FALSE,
    sep = ",",
    na = "",
    quote = FALSE
  )
  utils::write.table(
    rejected,
    file = paste(output_folder_path, "rejected.csv", sep=""),
    row.names = FALSE,
    sep = ",",
    na = "",
    quote = FALSE
  )
  
  # Also return the dataframes
  return(list(features=features, accepted=accepted, rejected=rejected))
}

################################################################################
# get_hourly_data()
################################################################################
get_hourly_data <- function(edf) {
  # Round dates to the nearest hour towards zero
  downtimes <- floor_date(edf$df$times, unit = "1 hour")
  # Aggregate data
  aggr_data <- stats::aggregate(
    x   = edf$df$values,
    by  = list(date_time = downtimes),
    FUN = sum
  )
  # Adapt to working dataframe
  edf$df <- data.frame(
    times  = aggr_data$date_time,
    values = aggr_data$x
  )
  # REDUCE SEASONAL PERIODS TO TWO VALUES
  edf$seasonal_periods <- c(24, 168)
  
  return(edf)
}

################################################################################
# post_features()
################################################################################
post_features <- function(o) {
  # List all subfeat files
  lof <- list.files(path=o, pattern="feats.+csv")
  # Sort by number!
  lof <- lof[order(readr::parse_number(lof))]
  # Get length
  len_lof <- length(lof)
  # If only 1 subfeat file
  if(len_lof == 1) {
    # Rename to definitive file
    file.rename(paste0(o,lof[1]), paste0(o,"feats.csv"))
  # More than 1 subfeat file
  } else {
    # Full-join all subfeat files
    o_df <- data.table::fread(paste0(o,lof[1]))
    for(ii in 2:len_lof) {
      new_df <- data.table::fread(paste0(o,lof[ii]))
      # Converting "fname" to character
      o_df   <- o_df   %>% mutate(fname = as.character(fname))
      new_df <- new_df %>% mutate(fname = as.character(fname))
      o_df   <- suppressMessages(full_join(o_df, new_df))
    }
    # Save results to the CSV file
    data.table::fwrite(
      x         = o_df,
      file      = paste0(o, "feats.csv"),
      sep       = ",",
      na        = "",
      quote     = FALSE,
      append    = FALSE,
      col.names = TRUE,
      row.names = FALSE
    )
    # Delete all subfeat files
    file.remove(paste0(o, lof))
  }
}

################################################################################
# get_features()
################################################################################
get_features <- function(input_folder, output_path, type_of_analysis = "extra",
                         max_feats = 7000, limited_to = NULL) {
  
  # Create folders if they do NOT exist
  if (!dir.exists(output_path)) dir.create(output_path)
  
  # List of file paths
  fpaths <- c()
  for (ii in 1:length(input_folder)) {
    # Get list of file paths
    fpaths <- c(
      fpaths,
      list.files(input_folder[ii], pattern="*.RData", full.names = T)
    )
  }
  
  packages <- c(
    "tsfeatures", "moments", "forecast", "stats", "lubridate", "stringr",
    "utils"
  )
  export <- c(
    "stat_moments", "quantiles", "stat_data_aggregates", "daily_acf", 
    "get_bins", "catch22_features", "get_seasonal_features_from_timeseries",
    "get_feature_names", "get_peak_times", "stat_data_aggregates",
    "get_expected_bins", "get_timeseries_from_cooked_dataframe",
    "get_extrema_dates_from_timeseries", "get_features_from_cooked_dataframe",
    "get_features_from_raw_datasets", "get_hourly_data"
  )
  
  # SEQUENCING
  if (is.null(limited_to)) {
    length_fpaths <- length(fpaths)
  } else {
    length_fpaths <- limited_to
  }
  SS_seq <- c(seq(0, length_fpaths, max_feats), length_fpaths)
  for (SS in 1:(length(SS_seq)-1)) {
    
    # Setup parallel backend to use many processors
    cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(cores, outfile = "")
    doParallel::registerDoParallel(cl)
    
    # Progress bar
    pb <- txtProgressBar(style=3)
    
    o <- foreach::foreach(
      x         = (SS_seq[SS]+1):SS_seq[SS+1],
      .combine  = rbind,
      .inorder  = TRUE,
      .packages = packages,
      .export   = export
    ) %dopar% {
    # for(x in 1:length(fpaths)) {
      
      # TO SKIP ERROR "object 'stat_moments' of mode 'function' was not found"
      .GlobalEnv$stat_moments         <- stat_moments
      .GlobalEnv$quantiles            <- quantiles
      .GlobalEnv$stat_data_aggregates <- stat_data_aggregates
      .GlobalEnv$daily_acf            <- daily_acf
      
      # Set progress bar
      setTxtProgressBar(pb, x/length_fpaths)
      
      # Select file name
      fpath <- fpaths[x]
      # fname <- strsplit(basename(fpath), split=".RData")[[1]]
      # print(fname)
      # Load extended dataframe
      load(fpath)
      # Set exceptions
      if (!edf$is_0) {
        # METADATA
        ff_meta <- edf[-c(1:6)]
        # AGGREGATE TO HOURLY DATA
        edf <- get_hourly_data(edf)
        # GET FEATURES
        ff_feats <- get_features_from_cooked_dataframe(
          cdf              = edf,
          type_of_analysis = type_of_analysis
        )
        # Incorporate filename as a column
        if (length(ff_meta) == 0) {
          o <- ff_feats
        } else {
          o <- cbind(ff_meta, ff_feats)
        }
      } else {
        NULL
      }
    }
    
    # Stop parallelization
    parallel::stopCluster(cl)
    
    cat("\n")
    
    # Save results to the CSV file
    data.table::fwrite(
      x         = o,
      file      = paste0(output_path, "feats_", SS_seq[SS+1], ".csv"),
      sep       = ",",
      na        = "",
      quote     = FALSE,
      append    = FALSE,
      col.names = TRUE,
      row.names = FALSE
    )
  }
  
  # Manage feature output files
  post_features(output_path)
}