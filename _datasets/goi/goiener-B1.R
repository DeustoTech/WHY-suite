# Given a folder with SIMEL files, this script generates a file per user.
# This version incorporates parallelization.

library(foreach)

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  g_input <- "C:/go2/"
  g_output <- "C:/go2_out/"
}
if (.Platform$OS.type == "unix") {
  g_input <- "/home/ubuntu/carlos.quesada/disk/go2/simel/"
  g_output <- "/home/ubuntu/carlos.quesada/disk/go2/raw2/"
}

### INITIALIZATIONS
# Counter 
counter <- 0

# Get list of filenames in dataset folder
filenames <- list.files(g_input)
total_fnames <- length(filenames)

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# File by file
foreach::foreach (ff = 1:total_fnames) %do% {
  # Get filename
  filename <- filenames[ff]
  # Load Goiener file
  g_df <- data.table::fread(
  	file       = paste0(g_input, filename),
  	header     = FALSE,
  	sep        = ";",
  	na.strings = ""
  )
  # Unique users in file
  unique_users <- unique(g_df$V1)
  foreach::foreach (uu = 1:length(unique_users)) %dopar% {
    un_us <- unique_users[uu]
  	# Select all un_us entries in df
  	uu_entries <- g_df$V1 == un_us
  	# Create new dataframe
  	browser()
  	uu_df <- data.frame(filename, g_df[uu_entries, ])
  	# Complete columns (to 35)
  	uu_df[,(ncol(uu_df)+1):35] <- NA
  	# Save
  	data.table::fwrite(
  	  x         = uu_df,
  	  file      = paste0(g_output, un_us, ".csv"),
  	  append    = TRUE,
  	  quote     = FALSE,
  	  sep       = ",",
  	  row.names = FALSE,
  	  col.names = FALSE,
  	  na        = ""
  	)
  }
}

# Stop parallelization
parallel::stopCluster(cl)
