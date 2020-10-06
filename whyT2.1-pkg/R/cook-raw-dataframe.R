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
  last_ts_date <- tail(raw_df, 1)[[1, 1]]

  # Check interval left end
  if (any(class(from_date) == "character")) {
    if (from_date == "first") {
      from_date <- raw_df[[1, 1]]
    }
  }
  # Check interval right end
  if (any(class(to_date) == "character")) {
    if (to_date == "last") {
      to_date <- tail(raw_df, 1)[[1, 1]]
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
