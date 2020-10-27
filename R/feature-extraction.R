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
    seasonal.periods = cdf$seasonal_periods,
    ts.frequency     = lowest_season, 
    start            = c(start_yearday, start_offset)
  )
  
  return(tseries)
}

#' Seasonal features from time series
#' 
#' @description 
#' Get the mean values (and variances) for all hours in a day, weekdays in a week and months in a year across a time series of class \code{msts}.
#' 
#' @param tseries Time series of class \code{msts}.
#' 
#' @return List of lists with the mean values and variances. 
#' 
#' @export

get_seasonal_features_from_timeseries <- function(tseries) {
  # Initial date
  ini_date <- get_extrema_dates_from_timeseries(tseries)
  # Date sequence
  samples_per_day <- attr(tseries, "msts")[1]
  date_by  <- as.difftime(24 / samples_per_day, units = "hours")
  date_seq <- seq(from       = ini_date,
                  length.out = length(tseries),
                  by         = date_by)
  
  # Loop initializations
  cut_breaks_list <- c("1 hour", "1 day", "1 month")
  name_list <- c(as.name("hourly"), as.name("daily"), as.name("monthly"))
  elems_per_bin_list <- c(
    samples_per_day / 24,
    samples_per_day,
    round(samples_per_day * 30.4375)
  )
  result_mean <- list()
  result_var  <- list()
  
  # Seasonality loop
  for (ii in 1:3) {
    ### Bin by periods of 1 month
    cut_seq <- cut(date_seq, breaks = cut_breaks_list[ii])
    # Aggregate data (sum) according to the bins
    aggr_ts <- stats::aggregate(
      x   = as.numeric(tseries),
      by  = list(date_time = cut_seq),
      FUN = sum )
    ## Estimate incomplete bins (initial and final bins)
    # Number of elements per bin
    elems_per_bin <- elems_per_bin_list[ii]
    # Number of elements in the first bin
    elems_first_bin <- sum(as.numeric(cut_seq) == 1)
    # Check for some estimation in the first bin
    skip_first <- FALSE
    if (elems_first_bin < elems_per_bin) {
      # Enough samples to perform estimation?
      if (elems_first_bin / elems_per_bin > 0.5) {
        aggr_ts[1,2] <- aggr_ts[1,2] * (elems_per_bin / elems_first_bin)
      } else {
        skip_first <- TRUE
      }
    }
    # Index of the last bin in aggr_ts
    id_last_bin <- dim(aggr_ts)[1]
    # Number of elements in the last bin
    elems_last_bin <- sum(as.numeric(cut_seq) == id_last_bin)
    # Check for some estimation in the last bin
    skip_last <- FALSE
    if (elems_last_bin < elems_per_bin) {
      # Enough samples to perform estimation?
      if (elems_last_bin / elems_per_bin > 0.5) {
        aggr_ts[id_last_bin, 2] <-
          aggr_ts[id_last_bin, 2] * (elems_per_bin / elems_last_bin)
      } else {
        skip_last = TRUE
      }
    }
    # Skip unestimated bins
    if (skip_first) {
      aggr_ts <- aggr_ts[2:dim(aggr_ts)[1],]
    }
    if (skip_last) {
      aggr_ts <- aggr_ts[1:(dim(aggr_ts)[1]-1),]
    }
    id_last_bin <- dim(aggr_ts)[1]
    ## Assign meaningful bin tags
    if (ii == 1) {
      nice_bins <- (lubridate::hour(as.POSIXct(aggr_ts[1,1])) + 
                      0:(id_last_bin - 1)) %% 24
    }
    if (ii == 2) {
      nice_bins <- (lubridate::wday(as.POSIXct(aggr_ts[1,1])) - 1 + 
                      0:(id_last_bin - 1)) %% 7 + 1
    }
    if (ii == 3) {
      nice_bins <- (lubridate::month(as.POSIXct(aggr_ts[1,1])) - 1 + 
                      0:(id_last_bin - 1)) %% 12 + 1
    }
    # Aggregate data (mean) according to the nice bins
    result_mean[[name_list[[ii]]]] <- stats::aggregate(
      x   = aggr_ts$x,
      by  = list(bin = nice_bins),
      FUN = mean
    )
    # Aggregate data (variance) according to the nice bins
    result_var[[name_list[[ii]]]]  <- stats::aggregate(
      x   = aggr_ts$x,
      by  = list(bin = nice_bins),
      FUN = stats::var
    )
  }
  
  return(list(mean = result_mean, var = result_var))
}

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
  initial_td <- as.POSIXct(paste(initial_date, initial_time), tz="GMT")
  
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
    final_td <- as.POSIXct(paste(final_date, final_date), tz="GMT")
    
    return(list(initial_date = initial_td, final_date = final_td))
  }
}

#' Features of a cooked dataframe
#'
#' @description
#' Get features of a cooked (or extended) dataframe.
#'
#' @param df Cooked (or extended) dataframe.
#' @param type_of_analysis A string indicating the type of analysis: either \code{basic} or \code{extra}. \code{basic} contains 7 functions whereas \code{extra} contains 33 (see code).
#'
#' @return List of features.
#'
#' @export

get_features_from_cooked_dataframe <- function(cdf, type_of_analysis) {
  # Set a seed for random numbers
  set.seed(1981)
  # List of functions that DON'T require normalization
  # They are included in BOTH "basic" and "extra" analyses
  not_norm_fns <- c("stat_moments", "quantiles", "load_factors")
  # List of BASIC functions that REQUIRE normalization
  basic_fns <- c("frequency", "stl_features", "entropy", "acf_features")
  # List of EXTRA functions that REQUIRE normalization
  extra_fns <- c("max_kl_shift", "outlierinclude_mdrmd", "arch_stat",
                 "max_level_shift", "ac_9", "crossing_points", "max_var_shift",
                 "nonlinearity", "spreadrandomlocal_meantaul", "flat_spots",
                 "pacf_features", "firstmin_ac", "std1st_der", "heterogeneity",
                 "stability", "firstzero_ac", "trev_num", "holt_parameters",
                 "walker_propcross", "hurst", "unitroot_kpss",
                 "histogram_mode", "unitroot_pp", "localsimple_taures",
                 "lumpiness", "motiftwo_entro3")
  # List of functions that require NORMALIZATION ("extra" includes "basic")
  analysis_fns <- list(
    basic = basic_fns,
    extra = c(basic_fns, extra_fns)
    )
  # Get multiseasonal time series
  tseries <- get_timeseries_from_cooked_dataframe(cdf)
  # Extract features that DON'T require normalization of the time series
  not_norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(tseries),
    features  = not_norm_fns,
    scale     = FALSE,   # <-- time series are NOT SCALED
    na.action = forecast::na.interp
  )
  # Extract features that DO require normalization of the time series
  norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(tseries),
    features  = analysis_fns[[type_of_analysis]],
    scale     = TRUE,    # <-- time series ARE SCALED to mean 0 and sd 1
    na.action = forecast::na.interp
  )
  # Bind features into a unique dataframe
  feats <- cbind(not_norm_feats, norm_feats)
  
  return(feats)
}

#' Features of raw datasets in a folder
#'
#' @description
#' Get features of all datasets contained in a folder.
#'
#' @param folder_path String with the absolute path to the dataset folder (ending in `/`).
#' @param from_date Initial date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR the string \code{first}.
#' @param to_date Final date and time of the interval. Either a \code{POSIXct} class in the GMT time zone OR the string \code{last}.
#' @param dset_key Key of the dataset.
#' @param allowed_na A numerical value between 0 and 1. It represents the maximum percentage of admissible \code{NA} values in the cooked dataframe for which the feature extraction is performed. The \code{NA} values will be STL-interpolated (using \code{forecast::na.interp}) prior to the feature extraction.
#' @param type_of_analysis A string indicating the type of analysis: either \code{basic} or \code{extra}.
#' @param output_folder_path String with the absolute path to the output folder (ending in `/`).
#'
#' @return List of dataframes of (1) extracted features and (2) accepted and (3) rejected files for feature extraction.
#'
#' @export

get_features_of_datasets_in_folder <- function(folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis, output_folder_path=NULL) {
  # Initialization of outputs
  features <- NULL
  accepted <- NULL
  rejected <- NULL
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(folder_path)[1:10]
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset
    print(dset_filename)
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
