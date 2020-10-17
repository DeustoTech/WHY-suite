#' PCA of features
#'
#' @description
#' Compute and plot PCA from a dataframe of features.
#'
#' @param feats_folder String with the path to the folder of the features file.
#' @param otp Vector of observations to plot. `NULL` indicates all observations.
#' @param ftp Vector of features to plot.
#'
#' @return A list with class `princomp`.
#'
#' @export

pca_from_features <- function(feats_folder, otp=NULL, ftp) {
  # Load data from CSV files
  feats <- utils::read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )

  # 'otp' conversion
  if (is.null(otp)) {
    otp <- 1:dim(feats)[1]
  }

  # PCA
  pca <- stats::prcomp(feats[otp, ftp], scale. = TRUE)

  return(pca)
}
