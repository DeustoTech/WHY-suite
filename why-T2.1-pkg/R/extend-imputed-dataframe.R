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
    days_diff <- (wday_ied - wday_fd) %% 7
    # Extraction interval
    initial_extract_date <- initial_extract_date - days(days_diff)
    final_extract_date <- initial_extract_date + months(extension_in_months)
    extract_idx <- idf$df[,1] > initial_extract_date &
      idf$df[,1] <= final_extract_date
    # Extracted times and values
    extracted_times <- idf$df[extract_idx, 1]
    extracted_values <- idf$df[extract_idx, 2]
    # Extended times
    extended_times <- seq(
      final_date,
      final_extract_date + years(1) + days(days_diff),
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
