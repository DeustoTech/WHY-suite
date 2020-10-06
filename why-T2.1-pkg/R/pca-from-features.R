#' PCA of features
#'
#' @description
#' Compute and plot PCA of the features of the datasets.
#'
#' @param feats_folder String with the path to the folder where the features are contained.
#' @param otp Vector of the observations to plot.
#' @param ftp Vector of the features to plot.
#' @param axis_x Integer indicating the principal component to be plotted as axis x.
#' @param axis_y Integer indicating the principal component to be plotted as axis y.
#' @param color_by_SE_vars Boolean indicating if plotted points must be colored according to the socioeconomical variables.
#' @param SE_data_file String with the path to the file with the socioeconomical variables.
#' @param get_point_identity Boolean indicating if plotted points must be identified by clicking on them.
#'
#' @return A list with class `princomp`.
#'
#' @export

pca_from_features <- function(feats_folder, otp, ftp, axis_x, axis_y, color_by_SE_vars, SE_data_file, get_point_identity) {
  # Load data from CSV files
  feats <- utils::read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )

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
    SE_indices <- match(analyzed_series, SE_vars[,1])
    # Get grouped ACORN
    grouped_ACORN <- SE_vars[SE_indices,4]
    grouped_ACORN[grouped_ACORN == "Affluent"]    = "green"
    grouped_ACORN[grouped_ACORN == "Comfortable"] = "orange"
    grouped_ACORN[grouped_ACORN == "Adversity"]   = "red"
    grouped_ACORN[grouped_ACORN == "ACORN-U"]     = "blue"
    # color
    color <- grouped_ACORN
  } else {
    color <- "blue"
  }

  # PCA
  pca <- stats::prcomp(feats[otp, ftp], scale. = TRUE)

  # Plot PCA
  plot(
    pca[["x"]][, axis_x],
    pca[["x"]][, axis_y],
    col = color,
    pch = "+",
    xlab = paste("PC #", axis_x, sep = ""),
    ylab = paste("PC #", axis_y, sep = "")
  )

  # Get point identification
  if (get_point_identity) {
    # Get points
    ts_ids <- graphics::identify(
      x = pca[["x"]][, axis_x],
      y = pca[["x"]][, axis_y],
      plot = FALSE
    )
    # Print points
    for (ts_id in ts_ids) {
      print(ts_id)
    }
  }

  return(pca)
}
