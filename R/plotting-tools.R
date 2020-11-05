################################################################################
# plot_dataframe
################################################################################

#' Plotting of a dataframe
#'
#' @description
#' Plot a dataframe (either raw or cooked).
#'
#' @param dset_data The dataframe to be plotted.
#' @param title Optional title (`NULL` by default).
#'
#' @return Plot of the dataset.
#'
#' @export

plot_dataframe <- function(dset_data, title=NULL) {

  # Check existence of a 3rd column to apply color
  if (dim(dset_data)[2] > 2) {
    # Color vector is blue by default
    coloring = rep("blue", dim(dset_data)[1])
    # Replace ex-NA by red
    coloring[dset_data[,3] == 1] = "red"
    coloring[dset_data[,3] == 2] = "darkgreen" #"green"
  } else {
    coloring = "blue"
  }

  p <- 
    # Data to be plotted
    ggplot2::ggplot(
      data    = dset_data,
      mapping = ggplot2::aes(x=times, y=values)
    ) +
    # Type of graph (line in this case)
    ggplot2::geom_line(color = coloring) +
    # Title
    ggplot2::ggtitle(title) +
    # Labels
    ggplot2::labs(x = "Date", y = "kWh") + 
    # Axis limits
    ggplot2::scale_y_continuous(limits = c(0,5))

  print(p)
}

################################################################################
# plot_pca
################################################################################

#' Plotting of PCA scores
#'
#' @description
#' Plot PCA scores from a dataframe of PCA scores.
#'
#' @param pca_sc A dataframe of PCA scores.
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

################################################################################
# plot_kmeans
################################################################################

#' Plotting of k-means results or elbow curve
#'
#' @description
#' Plot k-means results or elbow curve.
#'
#' @param km An object of class `kmeans`.
#' @param feats_df Dataframe to be clustered (features, PCA scores).
#' @param plot_clusters If `TRUE`, plot k-means clusters.
#' @param plot_elbow If `TRUE`, plot elbow curve.
#'
#' @return Plotting of the k-means results or elbow curve.
#'
#' @export

plot_kmeans <- function(km, feats_df=NULL, plot_clusters=FALSE, plot_elbow=FALSE) {
  # Plot kmeans
  if (plot_clusters) {
    f <- factoextra::fviz_cluster(
      km, 
      data            = feats_df,
      show.clust.cent = FALSE,
      geom            = "point",
      pointsize       = 1
    )
    plot(f)
  }
  
  # Plot elbow
  if (plot_elbow) {
    # Initializating values
    sum_of_squares <- c()
    cluster_number <- c()
    # Picking up values
    for (ii in 1:length(km)) {
      if (!is.null(km[[ii]])) {
        sum_of_squares <- c(sum_of_squares, km[[ii]]$tot.withinss)
        cluster_number <- c(cluster_number, ii)
      }
    }
    # Ploting elbow
    plot(
      x    = cluster_number, 
      y    = sum_of_squares, 
      col  = "blue", 
      type = "l",
      main = "Elbow curve",
      xlab = "Number of clusters",
      ylab = "Sum of squares"
    )
  }
}

################################################################################
# plot_features_library
################################################################################

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
