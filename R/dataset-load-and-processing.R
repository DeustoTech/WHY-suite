#' Raw dataframe from dataset
#'
#' @description
#' Get a raw (or extended) dataframe from a dataset contained in a CSV file.
#'
#' @param csv_file String with the absolute path to the CSV file containing the dataset.
#'
#' @return Raw (or extended) dataframe.
#'
#' @export

get_raw_dataframe_from_dataset <- function(csv_file) {
  # Load data from CSV file
  data <- data.table::fread(
    file = csv_file,
    header = FALSE,
    sep = ",",
    na.strings = ""
  )
  
  # Times
  times <- as.POSIXct(data$V1, tz="GMT")
  # Values
  values <- data$V2
  # 2 columns
  if (ncol(data) == 2) {
    return(data.frame(times, values))
  } else {
    # Imputed
    imputed <- data$V3
    # 3 columns
    if (ncol(data) == 3) {
      return(data.frame(times, values, imputed))
    } else {
      # Original times
      original_times <- as.POSIXct(data$V4, tz="GMT")
      # 4 columns
      if (ncol(data) == 4) {
        return(data.frame(times, values, imputed, original_times))
      }
    }
  }
}

#' @rdname get_raw_dataframe_from_dataset
#' @export
get_dataframe <- get_raw_dataframe_from_dataset

#' Cooked dataframe from raw dataframe
#'
#' @description
#' Cook a raw dataframe. Cooking consists in completing missing samples with NA values, removing extra samples (those not matching the sampling period), extracting a user-defined time interval out of the raw dataframe, and checking its validity to be feature-analyzed.
#'
#' @param raw_df Raw dataframe.
#' @param from_date Initial date and time of the interval. Either a `POSIXct` class in the GMT time zone OR a string `first`.
#' @param to_date Final date and time of the interval. Either a `POSIXct` class in the GMT time zone OR a string `last`.
#' @param dset_key String indicating the key of the dataset. `lcl` for `Low Carbon London`.
#' @param filename Filename.
#' @param acorn_path Path to the file with the ACORN values.
#'
#' @return List with the cooked dataframe, a vector of seasonal periods, a double with the percentage of `NA` values, and a boolean indicating if the dataframe values are all zeros. Optional members of the list are the filename and the ACORN value.
#'
#' @export

cook_raw_dataframe <- function(raw_df, from_date, to_date, dset_key, 
                               filename=NULL, acorn_path=NULL) {
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
  
  # Number of NA
  number_of_na <- sum(is.na(cooked_df[,2]))
  
  # Check if all values are 0
  cooked_df_is_0 <- all(cooked_df[!is.na(cooked_df[,2]),2] == 0.0)
  
  # Get ACORN value
  acorn         <- NULL
  acorn_grouped <- NULL
  if (!is.null(filename) & !is.null(acorn_path)) {
    acorn_df <- read.csv(file = acorn_path,
                         header = TRUE,
                         sep = ",",
                         na.strings = ""
    )
    acorn_tag     <- substr(filename, 1, 9)
    acorn         <- acorn_df[acorn_df[,1] == acorn_tag, 3]
    acorn_grouped <- acorn_df[acorn_df[,1] == acorn_tag, 4]
  }
  
  # Returned list
  output <- list(
    df               = cooked_df,
    dset_key         = dset_key,
    filename         = filename,
    acorn            = acorn,
    acorn_grouped    = acorn_grouped,
    seasonal_periods = seasonal_periods,
    number_of_na     = number_of_na,
    is_0             = cooked_df_is_0
  )
  return(output)
}

#' Imputed dataframe from cooked dataframe
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

#' Extended dataframe from imputed dataframe
#'
#' @description
#' Replicate the existing values of a periodic time series to obtain a time series with the desired length in months. Only imputed dataframes larger than 372 days can be extended.
#'
#' @param idf Imputed dataframe.
#' @param length_in_months Minimum number of months of the extended output.
#'
#' @return Extended dataframe, i.e. an imputed dataframe with a 3rd column indicating if each sample has been extended or not and a 4th column indicating the original date of the replicated sample.
#'
#' @export

extend_imputed_dataframe <- function(idf, length_in_months) {
  # Get current length in months of idf
  initial_date  <- idf$df[1,1]
  final_date    <- tail(idf$df, n=1)[[1]]
  wday_fd       <- lubridate::wday(final_date)
  idf_in_days   <- as.numeric(final_date - initial_date)
  idf_in_months <- idf_in_days/31
  # If idf is shorter than expected, extend
  if (idf_in_months > 12) {
    # Length of the extension in months
    extension_in_months <- ceiling(length_in_months - idf_in_months)
    if (extension_in_months > 0) {
      # Subtract one year
      initial_extract_date <- final_date - lubridate::days(365)
      # Check weekdays: if original ends on Monday, copy must start on Monday
      wday_ied  <- lubridate::wday(initial_extract_date)
      days_diff <- (wday_fd - wday_ied) %% 7
      # Extraction interval
      initial_extract_date <- initial_extract_date + 
                              lubridate::days(days_diff)
      final_extract_date   <- initial_extract_date + 
                              lubridate::days(31*extension_in_months)
      extract_idx          <- idf$df[,1] > initial_extract_date &
                              idf$df[,1] <= final_extract_date
      # Extracted times and values
      extracted_times  <- idf$df[extract_idx, 1]
      extracted_values <- idf$df[extract_idx, 2]
      # Extended times
      extended_times <- seq(
        from       = final_date,
        by         = as.difftime(86400/idf$seasonal_periods[1], units = "secs"),
        length.out = length(extracted_times) + 1
      )
      # Create the extended dataframe
      extended_df <- data.frame(
        times          = extended_times[2:length(extended_times)],
        values         = extracted_values,
        imputed        = 2,
        original_times = extracted_times
      )
      # Bind rows
      idf$df            <- dplyr::bind_rows(idf$df, extended_df)
      idf$number_of_ext <- length(extracted_times)
      if (length_in_months >= 24) {
        low_seas <- idf$seasonal_periods[1]
        idf$seasonal_periods <- c(low_seas, low_seas*7, low_seas*365)
      }
    }
    return(idf)
  } else {
    stop("372-days-long time series are required.")
  }
}

#' Extended datasets from folder of raw datasets
#'
#' @description
#' Compute the extended dataframes from an input folder of datasets and store them in an output folder.
#' 
#' @details Automatizes the following sequence for a whole folder: raw dataset -> raw dataframe -> cooked dataframe -> imputed dataframe -> extended dataframe -> extended dataset.
#'
#' @param input_folder Input folder of datasets.
#' @param output_folder Desired output folder of extended datasets.
#'
#' @return Extended dataframes.
#'
#' @export

extend_datasets <- function(input_folder, output_folder) {
  # Path to ACORN folder
  acorn_folder <- paste("G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/", 
                        "informations_households.csv", sep="")
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder)
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset and impute
    print(dset_filename)
    file_path <- paste(input_folder, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    cdf <- cook_raw_dataframe(raw_df     = rdf, 
                              from_date  = "first", 
                              to_date    = "last", 
                              dset_key   = "lcl", 
                              filename   = dset_filename, 
                              acorn_path = acorn_folder)
    # Get length
    initial_date <- cdf$df[1,1]
    final_date <- tail(cdf$df, n=1)[[1]]
    length_in_days <- as.numeric(final_date - initial_date)
    # If TS is longer than 372 days, impute; ELSE discard
    if (length_in_days > 372) {
      idf <- impute_cooked_dataframe(
        cdf       = cdf, 
        season    = cdf$seasonal_periods[1] * 7, 
        short_gap = cdf$seasonal_periods[1] / 3
      )
      # Expand if needed
      edf <- extend_imputed_dataframe(idf=idf, length_in_months=25)
      # Save dataframe in output folder
      path <- paste(output_folder, substr(dset_filename,1,9), sep="")
      save(edf, file=path)
      print("SAVED!")
    }
    else print("DISCARDED")
  }
}
