# CONVERT GOIENER USER DATA INTO GOIENER USABLE (TIMESTAMPED) RAW DATASETS

library(doParallel)

# Path to goiener_users folder
goiener_users_folder <- "/mnt/disk2/goiener_users/"
# Output path
goiener_output_folder <- "/mnt/disk2/goiener_csv/"
# Files in goiener users folder with no user data
fnames_others <- c("repetition_id.csv", "rep_id_analysis.Rdata")

# Time origin
time_origin <- as.POSIXct("1970-01-01", tz="UTC")
# File formats
file_formats <- c("P5D", "RF5D", "P4D", "P2D", "P1D", "F5D", "F1", "C2",
				  "C1", "B5D", "A5D")
# Get all filenames in folder
fnames_in_folder <- list.files(goiener_users_folder)
# Get all filenames in "repetition_id.csv"
fnames_in_rep_id <- data.table::fread(
  file       = paste(goiener_users_folder, "repetition_id.csv", sep=""),
  header     = FALSE,
  sep        = ";",
  na.strings = "",
  drop       = c(2)
)$V1
# Filenames with no repeated dates
fnames_ok <- setdiff(fnames_in_folder, c(fnames_in_rep_id, fnames_others))

# FUNCTION that extracts timestamp and value from a dataframe's row
extract_data_from_row <- function(x) {
  # Extract
  file_format <- file_formats[
	match(substr(x[[1]], 1, 2), substr(file_formats, 1, 2))
  ]
  # Select format of data extraction
  if (any(file_format == c("A5D"))) {
	dates  <- as.POSIXct(x[[3]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[4]])
	input  <- as.numeric(x[[5]]) / 1000
	output <- NA
  }
  if (any(file_format == c("B5D", "F5D", "P5D", "RF5D"))) {
	dates  <- as.POSIXct(x[[3]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[4]])
	input  <- as.numeric(x[[5]]) / 1000
	output <- as.numeric(x[[6]]) / 1000
  }
  if (any(file_format == c("F1"))) {
	dates  <- as.POSIXct(x[[4]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[5]])
	input  <- as.numeric(x[[6]]) 
	output <- as.numeric(x[[7]]) 
  }
  if (any(file_format == c("P1D", "P2D"))) {
	dates  <- as.POSIXct(x[[4]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[5]])
	input  <- as.numeric(x[[6]]) 
	output <- as.numeric(x[[8]]) 
  }
  return(c(dates, input, output))
}

# Setup parallel backend to use many processors
cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

# Given a file name, return dates, inputs and outputs
foreach (x = 1:length(fnames_ok)) %dopar% {
  # Read file
  g_df <- data.table::fread(
	file       = paste(goiener_users_folder, fnames_ok[x], sep=""),
	header     = FALSE,
	sep        = ",",
	na.strings = ""
  )
  # Dates in file
  dnv_mat <- t(apply(g_df, 1, extract_data_from_row))
  # Convert matrix to dataframe
  dnv_df <- as.data.frame(dnv_mat)
  # Sort dataframe by date
  dnv_df <- dplyr::arrange(dnv_df, V1)
  # Convert numbers to dates
  dnv_df$V1 <- as.POSIXct(dnv_df$V1, tz = "UTC", origin = time_origin)
  # Write file
  data.table::fwrite(
	dnv_df,
	file       = paste(goiener_output_folder, fnames_ok[x], ".csv", sep=""),
	row.names  = FALSE,
	col.names  = FALSE,
	sep        = ",",
	na         = "",
	dateTimeAs = "write.csv"
  )
}

parallel::stopCluster(cl)
