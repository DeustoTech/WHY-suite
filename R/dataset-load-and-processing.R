################################################################################
# get_raw_dataframe_from_dataset
################################################################################

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

################################################################################
# cook_raw_dataframe
################################################################################

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

################################################################################
# impute_cooked_dataframe
################################################################################

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

################################################################################
# extend_imputed_dataframe
################################################################################

#' Extended dataframe from imputed dataframe
#'
#' @description
#' Create an extended dataframe from an imputed dataframe by replicating previous sequences of the time series, of at least one year, as many times as necessary, until completing a certain number of days. 
#'
#' @param idf Imputed dataframe.
#' @param wanted_days Number of complete days of the extended output.
#' @param back_years Number of previous years for which the time series is copied.
#'
#' @return Extended dataframe, i.e. an imputed dataframe with a 3rd column indicating if each sample has been extended or not and a 4th column indicating the original date of the replicated sample.
#'
#' @export

extend_imputed_dataframe <- function(idf, wanted_days, back_years=1,  
                                     extend_after_end=TRUE) {
  # Get current length in months of idf
  idf_init_date  <- idf$df[1,1]
  idf_final_date <- tail(idf$df, n=1)[[1]]
  idf_days       <- as.numeric(idf_final_date - idf_init_date)
  # Enough days
  if (idf_days < 364) {
    return(NULL)
  }
  # Required days
  required_days  <- ceiling(wanted_days - idf_days)
  # Check if size is correct
  if (required_days <= 0) {
    return(idf)
  }
  # Number of NAs to append
  number_of_NAs <- required_days * idf$seasonal_periods[1]
  # Append NAs AFTER the end of the TS
  if (extend_after_end) {
    ext_vect <- c(idf$df[,2], rep(NA, times=number_of_NAs))
  # Append NAs BEFORE the beginning of the TS
  } else {
    ext_vect <- c(rev(idf$df[,2]), rep(NA, times=number_of_NAs))
  }
  # Create ts
  imp_ts <- ts(
    data      = ext_vect,
    frequency = back_years * 364 * idf$seasonal_periods[1]
  )
  # Index of new values
  new_val_idx <- is.na(imp_ts)
  extr_times  <- sum(new_val_idx)
  
  ##### IMPUTE #####
  imp_ts <- imputeTS::na_seasplit(imp_ts, algorithm  = "locf")
  
  # Reverse reverted 
  if (!extend_after_end) {
    imp_ts <- rev(imp_ts)
  }
  # Extended times
  extend_times <- seq(
    from       = idf_final_date,
    by         = as.difftime(86400/idf$seasonal_periods[1], units="secs"),
    length.out = extr_times + 1
  )
  # Create the extended dataframe
  extend_df <- data.frame(
    times   = extend_times[2:length(extend_times)],
    values  = imp_ts[new_val_idx],
    imputed = 2
  )
  # Bind rows
  idf$df <- dplyr::bind_rows(idf$df, extend_df)
  browser()
}

extend_imputed_dataframe.OLD <- function(idf, wanted_days, back_years=1) {
  # Get current length in months of idf
  idf_init_date  <- idf$df[1,1]
  idf_final_date <- tail(idf$df, n=1)[[1]]
  idf_final_wday <- lubridate::wday(idf_final_date)
  idf_days       <- as.numeric(idf_final_date - idf_init_date)
  
  # Count of loops of extension
  idf$extensions <- 1
  # Length of the extension in days
  extens_in_days <- ceiling(wanted_days - idf_days)
  # If idf is longer than required, do nothing!
  while (extens_in_days > 0) {
    ## Look for the exact point of extraction than matches the weekday
    # Subtract one year
    extr_init_date <- idf_final_date - lubridate::days(back_years*365)
    # Check weekdays: if original ends on Monday, copy must start on Monday
    extr_init_date_wday <- lubridate::wday(extr_init_date)
    # Get the shortest path of two
    diff_wdays <- c(
      (idf_final_wday - extr_init_date_wday) %% 7,
      - ((extr_init_date_wday - idf_final_wday) %% 7)
    )
    # Get the absolute minimum
    diff_wdays <- diff_wdays[which.min(abs(diff_wdays))][1]
    # Initial date of extraction
    extr_init_date <- extr_init_date + lubridate::days(diff_wdays)
    # Check if the initial date of extraction exists in the dataframe
    if (idf_init_date > extr_init_date) {
      return(NULL)
    }
    # Amount of extractable days
    available_days <- back_years*365 - diff_wdays
    # Check if the extension is larger than the amount of extractable days
    if (extens_in_days > available_days) {
      extr_final_date <- idf_final_date
      extens_in_days  <- extens_in_days - available_days
    } else {
      extr_final_date <- extr_init_date + lubridate::days(extens_in_days)
      extens_in_days  <- 0
    }
    # Extraction indices
    extr_idx <- idf$df[,1] > extr_init_date & idf$df[,1] <= extr_final_date
    # Extracted times and values
    extr_times  <- idf$df[extr_idx, 1]
    extr_values <- idf$df[extr_idx, 2]
    # Extended times
    extend_times <- seq(
      from       = idf_final_date,
      by         = as.difftime(86400/idf$seasonal_periods[1], units="secs"),
      length.out = length(extr_times) + 1
    )
    # Create the extended dataframe
    extend_df <- data.frame(
      times          = extend_times[2:length(extend_times)],
      values         = extr_values,
      imputed        = 2,
      original_times = extr_times
    )
    # Bind rows
    idf$df <- dplyr::bind_rows(idf$df, extend_df)
    idf$extensions <- idf$extensions + 1
    # New final dates if needed
    if (extens_in_days > 0) {
      idf_final_date <- tail(idf$df, n=1)[[1]]
      idf_final_wday <- lubridate::wday(idf_final_date)
    }
  }
  # Add new seasonal periods if needed
  if (wanted_days > 730) {
    low_seas <- idf$seasonal_periods[1]
    idf$seasonal_periods <- c(low_seas, low_seas*7, low_seas*365)
  }
  return(idf)
}

################################################################################
# extend_datasets
################################################################################

#' Extended datasets from folder of raw datasets
#'
#' @description
#' Compute the extended dataframes from an input folder of raw datasets and store them in an output folder.
#' 
#' @details Automatizes the following sequence for a whole folder: raw dataset -> raw dataframe -> cooked dataframe -> imputed dataframe -> extended dataframe -> extended dataset.
#'
#' @param input_folder Input folder of datasets.
#' @param output_folder Desired output folder of extended datasets.
#' @param wanted_days Minimum number of complete days of the final extended datasets.
#'
#' @return As many extended dataframes as files in the raw dataset folder.
#'
#' @export

extend_datasets <- function(input_folder, output_folder, wanted_days) {
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
    cdf <- cook_raw_dataframe(
      raw_df     = rdf, 
      from_date  = "first", 
      to_date    = "last", 
      dset_key   = "lcl", 
      filename   = dset_filename, 
      acorn_path = acorn_folder
      )
    # Get length
    initial_date   <- cdf$df[1,1]
    final_date     <- tail(cdf$df, n=1)[[1]]
    length_in_days <- as.numeric(final_date - initial_date)
    # If TS is longer than 365 days, impute; ELSE discard
    if (length_in_days > 365) {
      idf <- impute_cooked_dataframe(
        cdf       = cdf, 
        season    = cdf$seasonal_periods[1] * 7, 
        short_gap = cdf$seasonal_periods[1] / 3
      )
      # Expand if needed
      edf <- extend_imputed_dataframe(idf=idf, wanted_days=wanted_days)
      if (is.null(edf)) {
        print("DISCARDED")
      } else {
        # Save dataframe in output folder
        path <- paste(output_folder, substr(dset_filename,1,9), sep="")
        save(edf, file=path)
        print("SAVED!")
      }
    }
    else print("DISCARDED")
  }
}
