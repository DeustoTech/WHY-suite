#' Features of a cooked dataframe
#'
#' @description
#' Get features of a cooked dataframe.
#'
#' @param df Cooked dataframe.
#' @param type_of_analysis A string indicating the type of analysis: either `basic` or `extra`. `basic` contains 7 functions whereas `extra` contains 33.
#'
#' @return List of features.
#'
#' @export

get_features_from_cooked_dataframe <- function(df, type_of_analysis) {
  # Set a seed for random numbers
  set.seed(1981)
  # Analysis list
  not_norm_fns <- c("stat_moments", "quantiles", "electricity")
  basic_fns <- c("frequency", "stl_features", "entropy", "acf_features")
  extra_fns <- c("max_kl_shift", "outlierinclude_mdrmd", "arch_stat",
                 "max_level_shift", "ac_9", "crossing_points", "max_var_shift",
                 "nonlinearity", "spreadrandomlocal_meantaul", "flat_spots",
                 "pacf_features", "firstmin_ac", "std1st_der", "heterogeneity",
                 "stability", "firstzero_ac", "trev_num", "holt_parameters",
                 "walker_propcross", "hurst", "unitroot_kpss",
                 "histogram_mode", "unitroot_pp", "localsimple_taures",
                 "lumpiness", "motiftwo_entro3")
  analysis_fns <- list(basic = basic_fns,
                       extra = c(basic_fns, extra_fns))
  # Compute delay
  initial_date  <- df$df[1,1]
  lowest_season <- df$seasonal_periods[1]
  hour_factor   <- lowest_season / 24
  start_offset  <- 1 + lubridate::hour(initial_date) * hour_factor +
    lubridate::minute(initial_date) / 60 * hour_factor + 
    lubridate::second(initial_date) / 3600 * hour_factor
  
  # Convert to time series
  vals_msts <- forecast::msts(
    data             = df$df[,2],
    seasonal.periods = df$seasonal_periods,
    ts.frequency     = lowest_season,
    start            = c(1, start_offset)
  )
  # Extract features that DON'T require normalization of the time series
  not_norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(vals_msts),
    features  = not_norm_fns,
    scale     = FALSE,
    na.action = forecast::na.interp
  )
  # Extract features that DO require normalization of the time series
  norm_feats <- tsfeatures::tsfeatures(
    tslist    = list(vals_msts),
    features  = analysis_fns[[type_of_analysis]],
    scale     = TRUE,
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
#' @param from_date Initial date and time of the interval. Either a `POSIXct` class in the GMT time zone OR the string `first`.
#' @param to_date Final date and time of the interval. Either a `POSIXct` class in the GMT time zone OR the string `last`.
#' @param dset_key Key of the dataset.
#' @param allowed_na A numerical value between 0 and 1. It represents the maximum percentage of admissible `NA` values in the cooked dataframe for which the feature extraction is performed. The `NA` values will be STL-interpolated (using ``forecast::na.interp``) prior to the feature extraction.
#' @param type_of_analysis A string indicating the type of analysis: either `basic` or `extra`.
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
    raw_df <- get_raw_dataframe_from_dataset(file_path)
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
