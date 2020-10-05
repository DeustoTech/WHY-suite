# Carlos Quesada - Universidad de Deusto
# 2020.10.05
# Get features of all the datasets contained in a folder

library(whyT2.1)

#' Features of datasets in a folder
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
    if (cooked_df$na_percentage <= allowed_na & !cooked_df$is_0) {
      # GET FEATURES
      ff <- get_features_from_cooked_dataframe(cooked_df, type_of_analysis)
      # Incorporate features to output
      features <- dplyr::bind_rows(features, ff)
      # Create dataframe for accepted dataset
      aa <- data.frame(
        filename      = dset_filename,
        from_date     = format(from_date, "%Y-%m-%d %H:%M:%S"),
        to_date       = format(to_date, "%Y-%m-%d %H:%M:%S"),
        samples       = dim(cooked_df$df)[1],
        na_percentage = cooked_df$na_percentage
      )
      # Incorporate accepted dataframe to output
      accepted <- dplyr::bind_rows(accepted, aa)
      print("Features extracted!")
    }
    
    # Reject to extract features
    else {
      # Create dataframe for rejected dataset
      rr <- data.frame(
        filename      = dset_filename,
        from_date     = format(from_date, "%Y-%m-%d %H:%M:%S"),
        to_date       = format(to_date, "%Y-%m-%d %H:%M:%S"),
        na_percentage = cooked_df$na_percentage,
        is_0          = cooked_df$is_0
      )
      # Incorporate rejected dataframe to output
      rejected <- dplyr::bind_rows(rejected, rr)
      print("Features not extracted")
    }
  }
  
  # Save dataframes as CSV
  write.table(
    features,
    file = paste(output_folder_path, "features.csv", sep=""),
    row.names = FALSE,
    sep = ",",
    na = "",
    quote = FALSE
  ) 
  write.table(
    accepted,
    file = paste(output_folder_path, "accepted.csv", sep=""),
    row.names = FALSE,
    sep = ",",
    na = "",
    quote = FALSE
  )
  write.table(
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

folder_path      <- "G:/Mi unidad/WHY/Datasets/lcl/"
from_date        <- ISOdate(2013, 2, 1, 0, 0, 0)
to_date          <- ISOdate(2013, 2, 28, 23, 30, 0)
dset_key         <- "lcl"
allowed_na       <- 0
type_of_analysis <- "extra"

o <- get_features_of_datasets_in_folder(
  folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis)
