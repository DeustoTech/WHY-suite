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
#' @param from_date Initial date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR a string \code{first}.
#' @param to_date Final date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR a string \code{last}.
#' @param dset_key String indicating the key of the dataset. \code{lcl} for `Low Carbon London`.
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
#' Create an extended dataframe from an imputed dataframe by replicating previous sequences of the time series, of at least one year, as many times as necessary, until completing a certain number of days. It is consistent with the day of the week.
#'
#' @param idf Imputed dataframe.
#' @param wanted_days Number of complete days of the extended output.
#' @param back_years Number of previous years for which the time series is copied.
#' @param extend_after_end If \code{TRUE}, the expansion is appended after the end of the time series; if \code{FALSE}, the expansion is prepended before the beginning of the time series.
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
    ext_vect <- c(rep(NA, times=number_of_NAs), idf$df[,2])
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
  
  # Extend AFTER the end of the time series
  if (extend_after_end) {
    # Extended times
    extend_times <- seq(
      from       = idf_final_date,
      by         = as.difftime(86400/idf$seasonal_periods[1], units="secs"),
      length.out = extr_times + 1
    )
  # Extend BEFORE the beginning of the time series
  } else {
    # Initial date
    start_date <- idf_init_date - 
      as.difftime((extr_times+1)*86400/idf$seasonal_periods[1], units="secs")
    # Extended times
    extend_times <- seq(
      from       = start_date,
      by         = as.difftime(86400/idf$seasonal_periods[1], units="secs"),
      length.out = extr_times+1
    )
  }
  # Create the extended dataframe
  extend_df <- data.frame(
    times   = extend_times[2:length(extend_times)],
    values  = imp_ts[new_val_idx],
    imputed = 2
  )
  # Bind extension AFTER the time series
  if (extend_after_end) {
    idf$df <- dplyr::bind_rows(idf$df, extend_df)
  } else {
    idf$df <- dplyr::bind_rows(extend_df, idf$df)
  }
  return(idf)
}

################################################################################
# extend_lcl_dataset
################################################################################

#' Extension of "Low Carbon London" dataset files from folder of raw datasets
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

extend_lcl_dataset <- function(input_folder, output_folder, wanted_days) {
  # Path to ACORN folder
  metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
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
      acorn_path = metadata_file
      )
    # Get length
    initial_date   <- cdf$df[1,1]
    final_date     <- tail(cdf$df, n=1)[[1]]
    length_in_days <- as.numeric(final_date - initial_date)
    # If TS is longer than 364 days, impute; ELSE discard
    if (length_in_days >= 364) {
      idf <- impute_cooked_dataframe(
        cdf       = cdf, 
        season    = cdf$seasonal_periods[1] * 7, 
        short_gap = cdf$seasonal_periods[1] / 3
      )
      # Expand if needed
      edf <- extend_imputed_dataframe(idf=idf, wanted_days=wanted_days, extend_after_end=FALSE)
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

################################################################################
# extend_dataset
################################################################################

#' Extension of dataset files from folder of raw datasets
#'
#' @description
#' Compute the extended dataframes from an input folder of raw datasets and store them in an output folder.
#' 
#' @details Automatizes the following sequence for a whole folder: raw dataset -> raw dataframe -> cooked dataframe -> imputed dataframe -> extended dataframe -> extended dataset.
#'
#' @param input_folder Input folder of datasets.
#' @param output_folder Desired output folder of extended datasets.
#' @param wanted_days Minimum number of complete days of the final extended datasets.
#' @param dset_key Dataset key: \code{lcl} for Low Carbon London; \code{goi} for Goiener.
#' @param metadata_files Path or vector of paths to metadata files. 
#' @param from_date Initial date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR a string \code{first}.
#' @param to_date Final date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR a string \code{last}.
#'
#' @return As many extended dataframes as files in the raw dataset folder. The dataframes are saved as R files with extension \code{.RData}.
#'
#' @export

extend_dataset <- function(input_folder, output_folder, wanted_days, dset_key, metadata_files=NULL, from_date="first", to_date="last") {
  # Path to ACORN folder
  metadata_files <- metadata_files
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder)
  
  # Setup parallel backend to use many processors
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)
  
  # Analysis loop
  foreach::foreach (x = 1:length(dset_filenames)) %dopar% {
    # File name selection
    dset_filename <- dset_filenames[x]
    # Load raw dataframe from dataset and impute
    print(dset_filename)
    file_path <- paste(input_folder, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    cdf <- cook_raw_dataframe(
      raw_df     = rdf, 
      from_date  = from_date, 
      to_date    = to_date, 
      dset_key   = dset_key, 
      filename   = dset_filename, 
      acorn_path = metadata_files
    )
    # Get length
    initial_date   <- cdf$df[1,1]
    final_date     <- tail(cdf$df, n=1)[[1]]
    length_in_days <- as.numeric(final_date - initial_date)
    # If TS is longer than 364 days, impute; ELSE discard
    if (length_in_days >= 364) {
      idf <- impute_cooked_dataframe(
        cdf       = cdf, 
        season    = cdf$seasonal_periods[1] * 7, 
        short_gap = cdf$seasonal_periods[1] / 3
      )
      # How to extend the time series according to the dataset
      if (any(dset_key == c("lcl"))) {
        type_of_extension <- TRUE
      } else { # "goi"
        type_of_extension <- FALSE
      }
      # Expand if needed
      edf <- extend_imputed_dataframe(
        idf              = idf,
        wanted_days      = wanted_days,
        extend_after_end = type_of_extension
      )
      if (!is.null(edf)) {
        # Save dataframe in output folder
        path <- paste(
          output_folder,
          strsplit(dset_filename, ".csv")[[1]],
          ".RData",
          sep=""
        )
        save(edf, file=path)
      }
    }
  }
  
  parallel::stopCluster(cl)
}