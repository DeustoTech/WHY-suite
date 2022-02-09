# CONVERT GOIENER USER DATA INTO GOIENER USABLE (TIMESTAMPED) RAW DATASETS
# Improved version that corrects duplicated dates
# Corrects Daylight Savings Time!!! (2021.11.08)

library(foreach)
library(lubridate)

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  goiener_users_folder  <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.08 - users2raw issue/unsorted/user_test/"
  goiener_output_folder <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.08 - users2raw issue/unsorted/raw_test/"
}
if (.Platform$OS.type == "unix") {
  goiener_users_folder  <- "/home/ubuntu/carlos.quesada/disk/go4/users/"
  goiener_output_folder <- "/home/ubuntu/carlos.quesada/disk/go4/raw/"
}

# File formats
file_formats <- list(
  "A5" = 15, "B5" = 25, "C1" = 31, "C2" = 32, "F1" = 61,
  "F5" = 65, "P1" = 71, "P2" = 72, "P4" = 74, "P5" = 75, "RF" = 85
)
# Get all filenames in folders
u_fnames <- list.files(goiener_users_folder, pattern="^[[:xdigit:]]")
r_fnames <- list.files(goiener_output_folder, pattern="^[[:xdigit:]]")
# Subtract already computed filenames from filenames to be computed
fnames <- setdiff(u_fnames, r_fnames)
if (length(fnames) == 0){
 cat("All done!\n")
} else {
	# FUNCTION that groups of dupes
	align_users_dataframe <- function(dfe) {
	  ### Get file type id
	  # "A5" = 15, "B5" = 25, "C1" = 31, "C2" = 32, "F1" = 61,
	  # "F5" = 65, "P1" = 71, "P2" = 72, "P4" = 74, "P5" = 75, "RF" = 85
	  type_id <- file_formats[[substr(dfe[["V1"]], 1, 2)]]
	  # File version
	  file_version <- as.numeric(substr(dfe[["V1"]], 24, 24))
	  # Wrong type id? Exclude this entry!
	  if (is.null(type_id)) {
		  return(NULL)
	  }
	  if (type_id %in% c(31, 32, 74)) {
		  return(NULL)
	  }
	  ### Date-time, input, output & acquisition method
	  # A5D
	  if (type_id == 15) {
  		date_time <- lubridate::ymd_hm(dfe[["V3"]], tz="UTC") #- lubridate::hours(dfe[["V4"]])
  		input     <- as.numeric(dfe[["V5"]]) / 1000
  		output    <- 0
  		acq_meth  <- as.numeric(dfe[["V11"]])
	  }
	  # B5D
	  else if (type_id == 25) {
  		date_time <- lubridate::ymd_hm(dfe[["V3"]], tz="UTC") #- lubridate::hours(dfe[["V4"]])
  		input     <- as.numeric(dfe[["V5"]]) / 1000
  		output    <- as.numeric(dfe[["V6"]]) / 1000
  		acq_meth  <- as.numeric(dfe[["V11"]])
	  }
	  # F1
	  else if (type_id == 61) {
  		date_time <- lubridate::ymd_hms(dfe[["V4"]], tz="UTC") #- lubridate::hours(dfe[["V5"]])
  		input     <- as.numeric(dfe[["V6"]])
  		output    <- as.numeric(dfe[["V7"]])
  		acq_meth  <- as.numeric(dfe[["V14"]])
	  }
	  # F5D
	  else if (type_id == 65) {
  		date_time <- lubridate::ymd_hm(dfe[["V3"]], tz="UTC") #- lubridate::hours(dfe[["V4"]])
  		input     <- as.numeric(dfe[["V5"]]) / 1000
  		output    <- as.numeric(dfe[["V6"]]) / 1000
  		acq_meth  <- as.numeric(dfe[["V11"]])
	  }
	  # P1D
	  else if (type_id == 71) {
  		date_time <- lubridate::ymd_hms(dfe[["V4"]], tz="UTC") #- lubridate::hours(dfe[["V5"]])
  		input     <- as.numeric(dfe[["V6"]])
  		output    <- as.numeric(dfe[["V8"]])
  		acq_meth  <- as.numeric(dfe[["V22"]])
	  }
	  # P2D
	  else if (type_id == 72) {
  		date_time <- lubridate::ymd_hms(dfe[["V4"]], tz="UTC") #- lubridate::hours(dfe[["V5"]])
  		input     <- as.numeric(dfe[["V6"]])
  		output    <- as.numeric(dfe[["V8"]])
  		acq_meth  <- as.numeric(dfe[["V22"]])
	  }
	  # P5D
	  else if (type_id == 75) {
  		date_time <- lubridate::ymd_hm(dfe[["V3"]], tz="UTC") #- lubridate::hours(dfe[["V4"]])
  		input     <- as.numeric(dfe[["V5"]]) / 1000
  		output    <- as.numeric(dfe[["V6"]]) / 1000
  		acq_meth  <- 0
	  }
	  # RF5D
	  else if (type_id == 85) {
  		date_time <- lubridate::ymd_hm(dfe[["V3"]], tz="UTC") #- lubridate::hours(dfe[["V4"]])
  		input     <- as.numeric(dfe[["V5"]]) / 1000
  		output    <- as.numeric(dfe[["V6"]]) / 1000
  		acq_meth  <- as.numeric(dfe[["V11"]])
	  }

	  o <- data.frame(type_id, date_time, input, output, acq_meth, file_version)
	  return(o)
	}

	# FUNCTION that manage unique dates
	manage_uniq_dtimes <- function(dtime, df) {
	  # Index of unique date-time
	  idx <- which(df[,2] == dtime)
	  # Return
	  data.frame(
  		date_time = df[idx, 2],
  		input     = df[idx, 3],
  		output    = df[idx, 4]
	  )
	}

	# FUNCTION that manage duplicated dates
	manage_dupe_dtimes <- function(dtime, df) {
	  # Indices of repeated date-times
	  idx <- which(df[,2] == dtime)
	  ### TYPE 1: ANY FORMAT, EQUAL VALUES IN ALL FIELDS ###########################
	  if (length(unique(df[idx,3])) == 1 & length(unique(df[idx,4])) == 1) {
  		date_time <- df[idx[1], 2]
  		input     <- df[idx[1], 3]
  		output    <- df[idx[1], 4]
	  }
	  ### TYPE 2: SAME FORMAT, DIFFERENT ACQUISITION METHODS #######################
	  ### ALSO: SAME FORMAT, SAME ACQ. METHOD (HAPPENS WHEN DST) ###################
	  else if (length(unique(df[idx,1])) == 1 & !is.na(sum(df[idx,5]))) {
  		# Correct index
  		corr_idx  <- which(df[idx,5] == min(df[idx,5]))
  		date_time <- df[idx[1], 2]
  		input     <- mean(df[idx[corr_idx], 3])
  		output    <- mean(df[idx[corr_idx], 4])
	  }
	  ### TYPE 3: F5D AND/OR A5D AND/OR B5D ########################################
	  else if (
  		sum(df[idx, 1] == 15) <= 1 &
  		sum(df[idx, 1] == 25) <= 1 &
  		sum(df[idx, 1] == 65) <= 1 &
  		sum(df[idx, 1] == 15) + sum(df[idx, 1] == 25) + sum(df[idx, 1] == 65) >= 2
	  ) {
		  date_time <- df[idx[1], 2]
		  input     <- sum(df[idx, 3], na.rm=TRUE)
		  output    <- sum(df[idx, 4], na.rm=TRUE)
	  }
	  ### TYPE 4: P5D IS MORE ACCURATE THAN F5D (SINCE IT'S VALIDATED) #############
	  ### SEE https://www.boe.es/diario_boe/txt.php?id=BOE-A-2015-6203 #############
	  else if (
  		sum(df[idx, 1] == 75) >= 1 &
  		length(unique(df[idx[df[idx, 1] == 75], 3])) == 1 &
  		length(unique(df[idx[df[idx, 1] == 75], 4])) == 1 &
  		sum(df[idx, 1] == 65) >= 1 &
  		sum(df[idx, 1] == 75) + sum(df[idx, 1] == 65) == length(df[idx, 1])
	  ) {
  		  #print(df[idx,c(1,3,6)])
  		  date_time <- df[idx[1], 2]
  		  input     <- df[idx[df[idx, 1] == 75], 3][1]
  		  output    <- df[idx[df[idx, 1] == 75], 4][1]
	  }
	  ### TYPE 99: UNKNOWN #########################################################
	  else {
		  return(NULL)
	  }
	  
	  o <- data.frame(date_time, input, output)
	  return(o)
	}

	# Setup parallel backend to use many processors
	cores <- parallel::detectCores() - 1
	cl <- parallel::makeCluster(cores, outfile = "")
	doParallel::registerDoParallel(cl)
	
	# Progress bar
	pb <- txtProgressBar(style=3)
	# fnames length
	length_fnames <- length(fnames)
	# Given a file name, return dates, inputs and outputs
	out <- foreach (x = 1:length_fnames) %dopar% {
	#for (x in 1:length(fnames)) {
	  
	  # Set progress bar
	  setTxtProgressBar(pb, x/length_fnames)
	  # Read file
	  df <- data.table::fread(
  		file       = paste0(goiener_users_folder, fnames[x]),
  		header     = FALSE,
  		sep        = ",",
  		na.strings = NULL,
  		select     = c(1, 3:8, 11, 14, 22)
	  )
	  
	  # Align the dataframe
	  df <- apply(df, 1, align_users_dataframe)

	  if (!is.null(df)) {
  		# Aligned dataframe
  		df <- do.call(rbind, df)
  		# Identify repeated date-times
  		dtimes <- as.data.frame(table(df[[2]]))
  		uniq_dtimes <- as.POSIXct(dtimes[dtimes[,2] == 1, 1], tz="UTC")
  		dupe_dtimes <- as.POSIXct(dtimes[dtimes[,2] > 1, 1], tz="UTC")
  		# Manage duplicate dates
  		dupe_df <- do.call(rbind, lapply(dupe_dtimes, manage_dupe_dtimes, df = df))
  		# Manage unique dates
  		uniq_df <- do.call(rbind, lapply(uniq_dtimes, manage_uniq_dtimes, df = df))
  		# Final dataframe
  		final_df <- rbind(uniq_df, dupe_df)
  		# Sort by date
  		final_df <- final_df[order(final_df$date_time),]
  		# Write file
  		#print(paste0("SAVING: ", goiener_output_folder, fnames[x]))
  		data.table::fwrite(
  		  final_df,
  		  file       = paste0(goiener_output_folder, fnames[x]),
  		  row.names  = FALSE,
  		  col.names  = FALSE,
  		  sep        = ",",
  		  na         = "",
  		  dateTimeAs = "write.csv"
  		)
	  }
	}

	# Stop parallelization
	parallel::stopCluster(cl)
	cat("\n")
}
