#' Plotting of PCA scores
#'
#' @description
#' Compute and plot PCA from a dataframe of features.
#'
#' @param pca_sc A dataframe with PCA scores.
#' @param feats_folder String with the path to the folder of the features file.
#' @param axis_x Integer indicating the principal component to be plotted as axis x.
#' @param axis_y Integer indicating the principal component to be plotted as axis y.
#' @param color_by_SE_vars Boolean indicating if plotted points must be colored according to the socioeconomical variables.
#' @param SE_data_file String with the path to the file with the socioeconomical variables.
#'
#' @return Plotting of the PCA scores.
#'
#' @export

plot_pca <- function(pca_sc, feats_folder, axis_x, axis_y, color_by_SE_vars=FALSE, SE_data_file=NULL, get_point_identity=FALSE){
  # Color by socioeconomic variables or get identity of points?
  if (color_by_SE_vars | get_point_identity) {
    # Load data from CSV files
    data_info <- utils::read.table(
      file   = paste(feats_folder, "data_info.csv", sep = ""),
      header = TRUE,
      sep    = ","
    )
  }
  
  # Color by socioeconomic variables?
  if (color_by_SE_vars) {
    # Load data from CSV files
    SE_vars <- utils::read.table(
      file   = SE_data_file,
      header = TRUE,
      sep    = ","
    )
    # List of analyzed series
    analyzed_series <- substr(data_info[,1], 1, 9)
    # Find analyzed series in SE_vars
    SE_indices <- match(analyzed_series, SE_vars[, 1])
    # Get grouped ACORN
    color <- SE_vars[SE_indices, 4]
  } else {
    color <- NULL
  }
  
  # Plot PCA
  p <- 
    # Data to be plotted
    ggplot2::ggplot(
      data    = pca_sc,
      mapping = ggplot2::aes_q(
        x   = as.name(names(pca_sc)[axis_x]),
        y   = as.name(names(pca_sc)[axis_y]),
        col = color
      )
    ) +
    # Color palette
    ggplot2::scale_color_brewer(palette="Set1") +
    # Type of graph (line in this case)
    ggplot2::geom_point() + 
    # Title
    ggplot2::ggtitle("PCA scores") +
    # Labels
    ggplot2::labs(
      x    = paste("PC #", axis_x, sep = ""), 
      y    = paste("PC #", axis_y, sep = "")
    ) 
  
  print(p)
}

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
