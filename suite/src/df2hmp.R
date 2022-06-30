align_time_series <- function(fname, .scale) {
  # Load dataframe
  edf <- list()
  edf$df <- data.frame(data.table::fread(file=fname))
  # By hours
  t_factor <- cut(edf$df$times, breaks = "1 hour")
  # Aggregate by hour
  aggr_data <- aggregate(
    x   = edf$df$values,
    by  = list(date_time = t_factor),
    FUN = sum
  )
  # Check that upper whisker is not 0
  upwh <- upper_whisker(aggr_data$x)
  if(upwh == 0) return(NULL)
  # Scale data
  if(.scale) {
    aggr_data$x <- scale(
      aggr_data$x,
      center = FALSE,
      scale  = upwh
    )
  }
  # Input vector of dates
  i_times_vect <- lubridate::ymd_hms(aggr_data$date_time, tz="UTC")
  # Input vector of values
  i_value_vect <- aggr_data$x
  # Length of input vector
  i_len <- length(i_value_vect)
  # Number of years to be taken per time series (if "as much as possible" is
  # wanted, just set sp <- 0 and uncomment the commented while loop)
  nyears <- length(unique(lubridate::isoyear(i_times_vect)))
  # Length of one year
  len1yr <- 53*7*24
  # Output vector
  # o_times_vect <- rep(lubridate::as_datetime(NA), len1yr*nyears)
  o_value_vect <- rep(NA, len1yr*nyears)
  # Index of start of input vector
  i1 <- 1
  # Loop
  for(yy in 1:nyears) {
    # Get start index in output matrix
    o1 <- get_matrix_index(i_times_vect[i1])
    o1 <- o1 + (yy-1)*len1yr
    # Get end index in output matrix
    o2 <- yy*len1yr
    # Get end index of input vector
    i2 <- i1 + (o2 - o1)
    
    o_value_vect[o1:o2] <- i_value_vect[i1:i2]
    # Out of the loop if the sequence is finished
    if ((i2+1) > i_len) break
    # Find the new i1 (it must be Mon, 1 WK at 00:00h)
    aux_idx <- get_matrix_index(i_times_vect[i2+1])
    i1 <- (i2 + 1) - (aux_idx - 1)
  }
  # o_times_matx <- array(as.character(o_times_vect), dim = c(24,53*7,nyears))
  o_value_matx <- array(o_value_vect, dim = c(24,53*7,nyears))
  # # Compute the mean through array slices
  # o_mean_matx <- apply(o_value_matx, 2, rowMeans, na.rm=TRUE)
  # Return matrix
  return(o_value_matx)
}

get_heatmap_matrix <- function(fnames, .scale=TRUE) {
  # Align time series!: Get a list of arrays, one list element per file
  out <- lapply(fnames, align_time_series, .scale)
  # Turn list of arrays into big array
  out <- unlist(out)
  out <- array(out, dim=c(24,371,length(out)/8904))
  
  # Create matrix from means
  o_mean_mat <- apply(out, 2, rowMeans, na.rm=TRUE)
  # Flip matrix
  o_mean_mat <- o_mean_mat[nrow(o_mean_mat):1,]
  
  # Create matrix from medians
  o_median_mat <- apply(out, 2, matrixStats::rowMedians, na.rm=TRUE)
  # Flip matrix
  o_median_mat <- o_median_mat[nrow(o_median_mat):1,]
  
  # Create matrix from standard deviations
  o_sd_mat <- apply(out, 2, matrixStats::rowSds, na.rm=TRUE)
  # Flip matrix
  o_sd_mat <- o_sd_mat[nrow(o_sd_mat):1,]
  
  # Create matrix from MADs
  o_mad_mat <- apply(out, 2, matrixStats::rowMads, na.rm=TRUE)
  # Flip matrix
  o_mad_mat <- o_mad_mat[nrow(o_mad_mat):1,]
  
  # Coefficient of variance: just divide! 
  o_rsd_mat <- o_sd_mat / o_mean_mat
  # o_rsd_mat[!is.finite(o_rsd_mat)] <- NA
  # Coefficient of MAD (or whatever)
  o_rmad_mat <- o_mad_mat / o_median_mat
  # o_rmad_mat[!is.finite(o_rmad_mat)] <- NA
  
  return(list(mean=o_mean_mat, sd=o_sd_mat, rsd=o_rsd_mat,
              median=o_median_mat, mad=o_mad_mat, rmad=o_rmad_mat))
}

plot_heatmap_matrix <- function(
  m,
  format_file = "png",
  file_path   = paste0(getwd(), "/heatmap.png"),
  plot_width  = 800,
  plot_height = 600,
  subtitle    = NULL,
  col_palette = "YlOrRd"
) {
  
  # Format of output files
  if (format_file == "png")
    png(file_path, width = plot_width, height = plot_height)
  if (format_file == "pdf")
    pdf(file_path, width = plot_width, height = plot_height)
  
  # Plot heatmap
  image(
    t(m),
    useRaster = TRUE,
    axes      = FALSE,
    col       = hcl.colors(24, col_palette, rev = TRUE),
    zlim      = c(0, 1)
  )
  # Month labels
  m_labels <- rep(NA, 371)
  m_labels[round(seq(1, 371, length.out=25))[seq(2,25,by=2)]] <- month.abb[1:12]
  # Axis
  axis(1, at=seq(0, 1, length.out=371), labels=m_labels, las=0, tick=F)
  axis(2, at=seq(0, 1, length.out=24), labels=23:0, las=2, tick=F)
  # Title
  title(sub=subtitle)
  
  # Shutting down devices
  while(dev.off() != 1) {}
}

filename <- "C:/Users/carlos.quesada/Documents/WHY/2022.06.30 - Senyora de Renteria/47a9abcad0ff670439fbbdb350a1c76b097f2a2ad83b7a392852d8ecee53a5d7.csv"
m <- get_heatmap_matrix(filename)