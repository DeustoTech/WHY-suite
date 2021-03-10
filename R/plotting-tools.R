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
    ggplot2::labs(x = "Date", y = "kWh") #+ 
    # Axis limits
    #ggplot2::scale_y_continuous(limits = c(0,5))

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

################################################################################
# get_heatmap_matrix
################################################################################

get_heatmap_matrix <- function(fnames) {
  # FUNCTION FOR ALIGNING ANY TIME SERIES BY ISO WEEKS (1 TO 53)
  # INPUT: dataframe with pairs dataset-filename
  align_time_series <- function(fnames) {
    # Load dataframe
    path <- paste(
      "G:/Mi unidad/WHY/Datasets/",
      fnames[[1]],
      "-ext/",
      fnames[[2]],
      ".RData",
      sep = ""
    )
    load(path)
    # By hours
    t_factor <- cut(edf$df$times, breaks = "1 hour")
    # Aggregate by hour
    aggr_data <- stats::aggregate(
      x   = edf$df$values,
      by  = list(date_time = t_factor),
      FUN = sum
    )
    # Vector of dates
    date_vect <- as.POSIXct(aggr_data$date_time, tz="GMT")
    # Number of years to be taken per time series (if "as much as possible" is
    # wanted, just set sp <- 0 and uncomment the commented while loop)
    nyears <- 2
    # Length of the output vector
    lov <- 53*7*24
    # Starting point (is set to catch the central part of the time series
    # thus avoiding artificially extended ends)
    # sp <- 0 
    sp <- floor(length(date_vect)/2) - floor((nyears*lov)/2)
    # Output list
    li <- 0
    out_list <- list()
    # Loop
    # while(sp + lov <= length(date_vect)) {
    while(li < nyears) {
      # Create output vector
      out_vect <- rep(NA, lov)
      # Get time triad of initial time
      ini_week <-
        as.numeric(strftime(as.Date(date_vect[sp+1]), format = "%V"))
      ini_wday <- lubridate::wday(date_vect[sp+1], week_start=1)
      ini_hour <- lubridate::hour(date_vect[sp+1])
      # Position in the (53 x 7) x 24 vector
      ini_posv <- (ini_week-1)*7*24 + (ini_wday-1)*24 + (ini_hour+1)
      # Data pointer
      pp <- sp + lov - ini_posv + 1
      # Fill the tail of output vector
      out_vect[ini_posv:lov] <- aggr_data[(sp+1):pp,2]
      # Get time triad of final time
      fin_week <-
        as.numeric(strftime(as.Date(date_vect[pp+1]), format = "%V"))
      fin_wday <- lubridate::wday(date_vect[pp+1], week_start=1)
      fin_hour <- lubridate::hour(date_vect[pp+1])
      # Get new data pointers
      qq <- pp - (fin_week-1)*7*24 - (fin_wday-1)*24 + 1
      rr <- qq + ini_posv - 2
      # Fill the head of output vector
      out_vect[1:(ini_posv-1)] <- aggr_data[qq:rr,2]
      # Output list
      li <- li + 1
      out_list[[li]] <- out_vect
      # Move the starting point
      sp <- rr
    }
    return(out_list)
  }
  
  # Align time series!
  out_list <- apply(fnames, 1, align_time_series)
  # Unlist two layers
  out_list <- do.call(rbind,do.call(rbind,out_list))
  # Create matrix from means
  o_mat <- matrix(
    data = colMeans(out_list),
    nrow = 24
  )
  # Flip matrix
  o_mat <- o_mat[nrow(o_mat):1,]
  
  # # Week labels
  # x_labels <- rep(NA, 371)
  # x_labels[seq(1,371, by=7)] <- 1:53
  # # Plot heatmap
  # h <- heatmap(
  #   x       = o_mat,
  #   Rowv    = NA,
  #   Colv    = NA,
  #   labRow  = 23:0,
  #   labCol  = x_labels,
  #   scale   = "none",
  #   margins = c(2,0)
  # )
  
  return(o_mat)
}