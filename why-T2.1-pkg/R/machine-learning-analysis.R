#' PCA of features
#'
#' @description
#' Compute PCA from a dataframe of features.
#'
#' @param feats_folder String with the path to the folder of the features file.
#' @param otp Vector of observations to plot. `NULL` indicates all observations.
#' @param ftp Vector of features to plot.
#'
#' @return A list with class `princomp`.
#'
#' @export

pca_from_features <- function(feats_folder, ftp, otp=NULL) {
  # Load data from CSV files
  feats <- utils::read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )

  # otp conversion
  if (is.null(otp)) {
    otp <- 1:dim(feats)[1]
  }

  # PCA
  pca <- stats::prcomp(feats[otp, ftp], scale. = TRUE)
  return(pca)
}

#' k-means of features
#'
#' @description
#' Compute k-means from a dataframe of features.
#'
#' @param feats_folder String with the path to the folder of the features file.
#' @param ftp Vector of features to plot.
#' @param otp Vector of observations to plot. `NULL` indicates all observations.
#' @param centers Number of clusters.
#' @param iter_max Maximum number of iterations allowed.
#' @param nstart Number of random sets.
#'
#' @return Object of class `kmeans`.
#'
#' @export

kmeans_from_features <- function(feats_folder, ftp, otp=NULL, centers, iter_max=500, nstart=500) {
  # Load data from CSV files
  feats <- utils::read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )
  
  # otp conversion
  if (is.null(otp)) {
    otp <- 1:dim(feats)[1]
  }

  # Standard k-means method
  km <- stats::kmeans(
    x        = feats[otp, ftp],
    centers  = centers,
    iter.max = iter_max,
    nstart   = nstart
  )
  return(km)
}
