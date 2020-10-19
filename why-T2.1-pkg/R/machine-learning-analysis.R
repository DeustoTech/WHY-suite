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
  
  # Features 
  km_feats <- feats[otp, ftp]

  # Standard k-means method
  km_results <- list()
  for (cc in centers) {
    print(paste("Computing k-means for", cc, "clusters"))
    results <- stats::kmeans(
      x        = km_feats,
      centers  = cc,
      iter.max = iter_max,
      nstart   = nstart
    )
    km_results[[cc]] <- results
  }
  
  # Results
  km <- list(
    feats   = km_feats,
    results = km_results
  )
  
  return(km)
}
