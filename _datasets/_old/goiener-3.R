# This file analyzes the type of file that is being repeated

# Repetitions file path
goiener_users_folder <- "/mnt/disk2/goiener_users/"
repetition_id_file   <- "repetition_id.csv"

# Time origin
time_origin <- as.POSIXct("1970-01-01", tz="UTC")
# File formats
file_formats <- c("P5D", "RF5D", "P4D", "P2D", "P1D", "F5D", "F1", "C2",
				  "C1", "B5D", "A5D")
# FUNCTION that extracts the date from a dataframe's row
extract_date <- function(x) {
  # Extract
  file_format <- file_formats[
	match(substr(x[[1]], 1, 2), substr(file_formats, 1, 2))
  ]
  # Select format of data extraction
  if (any(file_format == c("P2D", "P1D", "F1"))) {
	dates <- as.POSIXct(x[[4]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[5]])
  }
  if (any(file_format == c("P5D", "RF5D", "F5D", "B5D", "A5D"))) {
	dates <- as.POSIXct(x[[3]], tz="UTC", origin=time_origin) - 
	  lubridate::hours(x[[4]])
  }
  return(c(file_format,dates))
}

# FUNCTION that, given a file name, returns the types of the duplicates
get_duplicate_types <- function(x) {
  print(x)
  # Read file
  g_df <- data.table::fread(
	file = paste(goiener_users_folder, x, sep=""),
	header = FALSE,
	sep = ",",
	na.strings = ""
  )
  # Dates in file
  types_and_dates <- apply(g_df, 1, extract_date)
  types <- types_and_dates[1,]
  dates <- as.POSIXct(as.numeric(types_and_dates[2,]),
					  tz = "UTC",
					  origin = time_origin)
  rm(types_and_dates)
  # Get indices of duplicates
  dupes_idx_1 <- which(duplicated(dates))
  dupes_idx_2 <- which(duplicated(dates, fromLast = TRUE))
  # Get types of duplicates
  dupes_types_1 <- unique(types[dupes_idx_1])
  dupes_types_2 <- unique(types[dupes_idx_2])
  # Return
  return(list(type_1=dupes_types_1, type_2=dupes_types_2))
}

# Load list of files with repetitions
rep_id <- data.table::fread(
  file = paste(goiener_users_folder, repetition_id_file, sep=""),
  header = FALSE,
  sep = ";",
  na.strings = "",
  drop = c(2)
)
rep_id <- as.vector(rep_id)$V1

# Analyze file by file
dupes <- lapply(rep_id, get_duplicate_types)
# Return
res <- data.frame(Reduce(rbind, dupes), row.names = NULL)
print(res)
save(res, file = paste(goiener_users_folder, "rep_id_analysis.Rdata", sep=""))
return(res)
