#' Creation of a PDF library of features
#'
#' @description
#' For each feature, the nine most representative values are plotted, including the minimum, the median and the maximum values.
#'
#' @param sampling_period Sampling period of the dataframe.
#' @param feats_folder String with a path to the folder where the features are contained.
#' @param feats_to_plot Vector of the features to be plotted.
#'
#' @return A PDF file per feature with nine plots each.
#'
#' @export

plot_features_library <- function(sampling_period, feats_folder, feats_to_plot) {
  # Libraries
  library(plotly)

  # Load data from CSV files
  feats <- read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )
  data_info <- read.table(
    file   = paste(feats_folder, "data_info.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )

  # Indices of series to be plotted
  seq_idx <- seq(1, nrow(feats), length=9)

  for (ii in feats_to_plot) {
    print(ii)
    # Feature name
    feat_name <- names(feats)[ii]
    # Get representative filenames of this feature ii
    sorted_col    <- sort(feats[[ii]], index.return = TRUE)
    selected_idx  <- sorted_col$ix[seq_idx]
    repres_fnames <- data_info$filename[selected_idx]
    repres_from_D <- as.POSIXct(data_info$from_date[selected_idx], tz="GMT")
    repres_to_D   <- as.POSIXct(data_info$to_date[selected_idx], tz="GMT")
    repres_values <- sorted_col$x[seq_idx]

    # Plot in PDF file
    pdf(
      file   = paste(ii, " - ", feat_name, ".pdf", sep = ""),
      paper  = "special", #"a4r",
      width  = 20,
      height = 15
    )
    par(mfrow = c(3,3))

    for (jj in 1:9) {
      # Load dataset file
      csv_path <- paste(DATASET_PATH, repres_fnames[jj], sep="")
      raw_df <- get_raw_dataframe_from_dataset(csv_path)
      # Get values from dataset file
      cooked_df <- cook_raw_dataframe(
        raw_df    = raw_df,
        from_date = repres_from_D[jj],
        to_date   = repres_to_D[jj],
        dset_key  = "lcl"
      )
      # Plot interval
      plot_dataframe(
        dset_data = cooked_df,
        title     = paste(feat_name, "=", repres_values[jj])
      )
    }
    dev.off()
  }
}
