#' Raw dataframe from dataset
#'
#' @description
#' Get a dataframe from a dataset contained in a CSV file.
#'
#' @param csv_file String with the absolute path to the CSV file containing the dataset.
#'
#' @return Raw dataframe, i.e. without checking missing samples.
#'
#' @export

get_raw_dataframe_from_dataset <- function(csv_file) {
  # Load data from CSV file
  data <- data.table::fread(
    file = csv_file,
    header = FALSE,
    sep = ","
  )
  # Times
  times <- as.POSIXct(data$V1, tz="GMT")
  # Values
  values <- data$V2
  # Create dataframe
  return(data.frame(times = times, values = values))
}

#' Cooked dataframe from raw dataframe
#'
#' @description
#' Cook a raw dataframe. Cooking consists in completing missing samples with NA values, removing extra samples (those not matching the sampling period), extracting a user-defined time interval out of the raw dataframe, and checking its validity to be feature-analyzed.
#'
#' @param raw_df Raw dataframe.
#' @param from_date Initial date and time of the interval. Either a `POSIXct` class in the GMT time zone OR a string `first`.
#' @param to_date Final date and time of the interval. Either a `POSIXct` class in the GMT time zone OR a string `last`.
#' @param dset_key String indicating the key of the dataset. `lcl` for `Low Carbon London`.
#'
#' @return List with the cooked dataframe, a vector of seasonal periods, a double with the percentage of `NA` values, and a boolean indicating if the dataframe values are all zeros.
#'
#' @export

cook_raw_dataframe <- function(raw_df, from_date, to_date, dset_key) {
  # List of samples per day (REMARK: ADD AS NEEDED!)
  SAMPLES_PER_DAY <- list(lcl = 48)
  # Selection
  spd <- SAMPLES_PER_DAY[[dset_key]]
  
  # Time series ends
  first_ts_date <- raw_df[[1, 1]]
  last_ts_date <- utils::tail(raw_df, 1)[[1, 1]]
  
  # Check interval left end
  if (any(class(from_date) == "character")) {
    if (from_date == "first") {
      from_date <- raw_df[[1, 1]]
    }
  }
  # Check interval right end
  if (any(class(to_date) == "character")) {
    if (to_date == "last") {
      to_date <- utils::tail(raw_df, 1)[[1, 1]]
    }
  }
  
  # Sampling period in seconds obtained from the dataset key
  sampling_period_in_secs <- 86400 / spd
  # Step as difftime
  period_in_secs <- as.difftime(sampling_period_in_secs, units = "secs")
  
  # Create time sequence
  time_seq <- seq(from_date, to_date, period_in_secs)
  # Find matches in dataframe (this completes missing samples with NA values
  # and removes extra samples out of the sampling period)
  mm <- match(time_seq, raw_df$times)
  # Output
  cooked_df <- data.frame(times = time_seq, values = raw_df$values[mm])
  
  # Get seasonal periods
  seasonal_periods <- NULL
  cooked_df_length <- dim(cooked_df)[1]
  # Days
  if (cooked_df_length > 2 * spd) {
    seasonal_periods <- c(seasonal_periods, spd)
  }
  # Weeks
  if (cooked_df_length > 2 * 7 * spd) {
    seasonal_periods <- c(seasonal_periods, 7 * spd)
  }
  # Years
  if (cooked_df_length > 2 * 365 * spd) {
    seasonal_periods <- c(seasonal_periods, 365 * spd)
  }
  
  # NA percentage
  na_percentage <- sum(is.na(cooked_df[,2])) / cooked_df_length
  
  # Check if all values are 0
  cooked_df_is_0 <- all(cooked_df[!is.na(cooked_df[,2]),2] == 0.0)
  
  # Returned list
  output <- list(
    df               = cooked_df,
    seasonal_periods = seasonal_periods,
    na_percentage    = na_percentage,
    is_0             = cooked_df_is_0
  )
  return(output)
}

#' Imputation of missing samples in cooked dataframe
#'
#' @description
#' Impute missing samples in a cooked dataframe. It can use an algorithm for short gaps (e.g. "interpolation") and another one for longer gaps (e.g. "locf").
#'
#' @param cdf Cooked dataframe.
#' @param season Seasonal period (e.g. 1 week) in number of samples.
#' @param short_gap Number of samples considered as a short gap.
#' @param short_algorithm Algorithm used to impute short gaps.
#' @param long_algorithm Algorithm used to impute long gaps.
#'
#' @return Imputed dataframe, i.e. a cooked dataframe with a 3rd column indicating if each sample has been imputed or not.
#'
#' @export

impute_cooked_dataframe <- function(cdf, season, short_gap, short_algorithm="interpolation", long_algorithm="locf") {
  # Time series pending imputation
  not_imp_ts <- ts(data=cdf$df[,2], frequency=season) # 1 week
  # Imputed time series
  imp_ts <- imputeTS::na_seasplit(not_imp_ts,
                                  algorithm = short_algorithm,
                                  maxgap = short_gap)
  imp_ts <- imputeTS::na_seasplit(imp_ts,
                                  algorithm = long_algorithm)
  # Imputed dataframe
  cdf$df <- data.frame(times   = cdf$df[,1],
                       values  = as.double(imp_ts),
                       imputed = as.integer(is.na(not_imp_ts)))
  return(cdf)
}

#' Extension of time series in imputed dataframe
#'
#' @description
#' Replicate the existing values of a periodic time series to obtain a time series with the desired length in months.
#'
#' @param idf Imputed dataframe.
#' @param length_in_months Minimum number of months of the extended output.
#'
#' @return Imputed dataframe, i.e. a cooked dataframe with a 3rd column indicating if each sample has been imputed or not.
#'
#' @export

extend_imputed_dataframe <- function(idf, length_in_months) {
  # Get current length in months of idf
  initial_date <- idf$df[1,1]
  final_date <- tail(idf$df, n=1)[[1]]
  wday_fd <- wday(final_date)
  idf_in_days <- as.numeric(final_date - initial_date)
  idf_in_months <- idf_in_days/30.4167
  # If idf is shorter than expected, extend
  if (idf_in_months < length_in_months) {
    # Length of the extension in months
    extension_in_months <- ceiling(length_in_months - idf_in_months)
    # Subtract one year
    initial_extract_date <- final_date - years(1)
    # Check the weekdays: if original ends on Monday, copy must start on Monday
    wday_ied <- wday(initial_extract_date)
    days_diff <- (wday_fd - wday_ied) %% 7
    # Extraction interval
    initial_extract_date <- initial_extract_date + days(days_diff)
    final_extract_date <- initial_extract_date + months(extension_in_months)
    extract_idx <- idf$df[,1] > initial_extract_date &
      idf$df[,1] <= final_extract_date
    # Extracted times and values
    extracted_times <- idf$df[extract_idx, 1]
    extracted_values <- idf$df[extract_idx, 2]
    # Extended times
    extended_times <- seq(
      final_date,
      final_extract_date + years(1) - days(days_diff),
      as.difftime(86400/idf$seasonal_periods[1], units = "secs")
    )
    # Create the extension dataframe
    extended_df <- data.frame(
      times          = extended_times[2:length(extended_times)],
      values         = extracted_values,
      imputed        = 2,
      original_times = extracted_times
    )
    # Bind rows
    idf$df <- dplyr::bind_rows(idf$df, extended_df)
    return(idf)
  } else {
    warning("Time series is not large enough to be extended (1 year required)")
  }
}
