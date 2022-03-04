### Check the repeated dates in the files

library(foreach)

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  g_input <- "C:/go2_raw2/"
  g_output <- "C:/go2_raw2/"
}
if (.Platform$OS.type == "unix") {
  g_input <- "/home/ubuntu/carlos.quesada/disk/go2/raw/"
  g_output <- "/home/ubuntu/carlos.quesada/disk/go2/"
}

# Get list of filenames in dataset folder
filenames <- list.files(g_input)
total_fnames <- length(filenames)
# File formats
file_formats <- c("P5D", "RF5D", "P4D", "P2D", "P1D", "F5D", "F1", "C2", "C1", "B5D", "A5D")
# Repeated elements
rep_df <- data.frame()

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# File by file
rep <- foreach::foreach (ff = 1:total_fnames, .combine="rbind") %do% {
  # Get filename
  filename <- filenames[ff]
  # Load Goiener file
  g_df <- data.table::fread(
  	file       = paste0(g_input, filename),
  	header     = FALSE,
  	sep        = ",",
  	na.strings = "",
  	select     = 1:5,
  	fill       = TRUE
  )
  # Get file sources
  file_sources <- unique(g_df$V1)
  # File sources loop
  date_list <- foreach::foreach (fs = 1:length(file_sources), .combine="c") %dopar% {
    file_source <- file_sources[fs]
  	# Get file format
  	file_format <- file_formats[
  	  match(substr(file_source, 1, 2), substr(file_formats, 1, 2))
  	]
  	# Index of this file source
  	idx <- g_df$V1 == file_source
  	# Date in #3, flag in #4
  	if (any(file_format == c("P5D", "RF5D", "F5D", "B5D", "A5D"))) {
  	  dates <- as.POSIXct(g_df[idx,]$V3, tz="UTC") - lubridate::hours(g_df[idx,]$V4)
  	}
  	# Date in #4, flag in #5
  	if (any(file_format == c("P2D", "P1D", "F1"))) {
  	  dates <- as.POSIXct(g_df[idx,]$V4, tz="UTC") - lubridate::hours(g_df[idx,]$V5)
  	}
  	return(dates)
  }

  # Check repeated 
  rep_elems <- which(duplicated(date_list))
  # Add repeated to dataframe
  if (length(rep_elems) != 0) {
    rep_df <- data.frame(file = filename)
  } else {
    rep_df <- data.frame()
  }
  return(rep_df)
}

# Stop parallelization
parallel::stopCluster(cl)

# Save
if (length(rep) != 0) {
  data.table::fwrite(
    x         = rep,
    file      = paste0(g_input, "raw_rep_dates.csv"),
    append    = TRUE,
    quote     = FALSE,
    sep       = ";",
    row.names = FALSE,
    col.names = FALSE,
    na        = ""
  )
}
