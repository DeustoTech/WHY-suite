# Given a folder with SIMEL files, this script generates a file per user.

### USER DEFINED VARIABLES
# Goiener data input folder
g_input <- "/mnt/disk2/goiener_raw/"
# Goiener data output folder
g_output <- "/mnt/disk2/goiener_users/"

### INITIALIZATIONS
# Counter 
counter <- 0

# Get list of filenames in dataset folder
filenames <- list.files(g_input)
total_fnames <- length(filenames)

# File by file
for (filename in filenames) {
  # Counter
  counter <- counter + 1
  # Print file being analyzed
  print(paste(date(),
			  filename,
			  round(100*counter/total_fnames, 4), 
			  sep=" | "))
  # Load Goiener file
  g_df <- data.table::fread(
	file = paste(g_input, filename, sep=""),
	header = FALSE,
	sep = ";",
	na.strings = ""
  )
  # Unique users in file
  unique_users <- unique(g_df$V1)
  for (uu in unique_users) {
	# Select all uu entries in df
	uu_entries <- g_df$V1 == uu
	# Create new dataframe
	uu_df <- data.frame(filename, g_df[uu_entries, ])
	# Save
	data.table::fwrite(
	  x = uu_df,
	  file = paste(g_output, uu, sep=""),
	  append = TRUE,
	  quote = FALSE,
	  sep = ",",
	  row.names = FALSE,
	  col.names = FALSE,
	  na = ""
	)
  }
}
