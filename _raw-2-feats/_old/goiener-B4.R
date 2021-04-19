# CONVERT GOIENER USER DATA INTO GOIENER USABLE (TIMESTAMPED) RAW DATASETS
# Improved version that corrects duplicated dates

library(foreach)

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  goiener_users_folder  <- "C:/go2_users/"
  goiener_output_folder <- "C:/go2_raw/"
}
if (.Platform$OS.type == "unix") {
  goiener_users_folder  <- "/home/ubuntu/carlos.quesada/disk/go2/users/"
  goiener_output_folder <- "/home/ubuntu/carlos.quesada/disk/go2/raw/"
}

# Time origin
time_origin <- as.POSIXct("1970-01-01", tz="UTC")
# File formats
file_formats <-
  c("P5D", "RF5D", "P4D", "P2D", "P1D", "F5D", "F1", "C2", "C1", "B5D", "A5D")
# Get all filenames in folder
fnames <- list.files(goiener_users_folder, pattern="^[[:xdigit:]]")

# FUNCTION that extracts timestamp and value from a dataframe's row
extract_data_from_row <- function(x) {
  # Extract
  file_format <- x[[1]]
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
  if (any(file_format == c("NULL"))) {
    return(c(NA, NA, NA))
  }
  return(c(dates, input, output))
}

# FUNCTION that treats NA values as if they were regular numbers
compareNA <- function(v1,v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# FUNCTION that acts as is.empty()
is_num <- function(x) {
  if (is.na(as.numeric(x)))
    return(0)
  else
    return(as.numeric(x))
}

# FUNCTION that reduces the file name to a clearer format
get_file_format <- function(df) {
  # File format
  df[,1] <- file_formats[
    match(substr(df[[1]], 1, 2), substr(file_formats, 1, 2))
  ]
  return(df)
}

# FUNCTION that manage duplicate dates in files
manage_duplicates <- function(df, file_name) {
  # Dates
  date_idx_B <- df[["V1"]] %in% c("A5D", "B5D", "F5D", "P5D", "RF5D")
  date_idx_C <- df[["V1"]] %in% c("F1", "P1D", "P2D")
  dates <- as.POSIXct(vector())
  dates[date_idx_B] <- as.POSIXct(df$V3, tz="GMT", origin=time_origin) -
    lubridate::hours(df$V4)
  dates[date_idx_C] <- as.POSIXct(df$V4, tz="GMT", origin=time_origin) -
    lubridate::hours(df$V5)
  # Boolean vector of duplicates
  dupes <- duplicated(dates)
  # Loop to remove duplicates
  while(sum(dupes) > 0) {
    # Type checker
    done <- FALSE
    # Indices of the first duplicates in the list
    idx <- which(dates == dates[which(dupes == TRUE)[1]])
    ### TYPE 1: ANY FORMAT, EQUAL VALUES IN ALL FIELDS #########################
    val_i <- c()
    val_o <- c()
    for (ii in 1:length(idx)) {
      if (df[[ii, 1]] == "B5D" | df[[ii, 1]] == "F5D" | df[[ii, 1]] == "P5D" | df[[ii, 1]] == "RF5D") {
        val_i <- c(val_i, df[[ii, 5]])
        val_o <- c(val_o, df[[ii, 6]])
      }
      if (df[[ii, 1]] == "P1D" | df[[ii, 1]] == "P2D") {
        val_i <- c(val_i, df[[ii, 6]])
        val_o <- c(val_o, df[[ii, 8]])
      }
      if (df[[ii, 1]] == "A5D") {
        val_i <- c(val_i, df[[ii, 5]])
        val_o <- c(val_o, 0)
      }
    }
    # Check if all values are identical
    if (length(unique(val_i)) == 1 & length(unique(val_o)) == 1) {
      # Mark as null entry
      df[idx[-1], 1] <- "NULL"
      # Dupe index modification
      dupes[idx] <- FALSE
      # Type checker
      done <- TRUE
    }
    
    ### TYPE 2: FORMAT F5D, DIFFERENT VALUES IN FIELD J ########################
    # Check for format = input <- D & output <- E
    if (all(df[idx, 1] == "F5D")) {
      # Check if J-fields are different
      if (dim(unique(df[idx, 11]))[1] > 1) {
        # Minimum entries
        min_idx <- which(df[idx, 11] == min(df[idx, 11]))
        # Mark as null entry
        dates[idx[-min_idx]] <- NA
        df[idx[-min_idx], 1] <- "NULL"
        # Dupe index modification
        dupes[idx[-min_idx]] <- FALSE
        # Type checker
        done <- TRUE
      }
    }
    
    ### TYPE 3: F5D AND/OR A5D AND/OR B5D ######################################
    # Check for format = input <- D & output <- E
    if (sum(df[idx, 1] == "F5D") <= 1 &
        sum(df[idx, 1] == "A5D") <= 1 &
        sum(df[idx, 1] == "B5D") <= 1 &
        sum(df[idx, 1] == "F5D") + sum(df[idx, 1] == "A5D") +
        sum(df[idx, 1] == "B5D") == length(idx)) {
      # Flags to identify formats
      a5d_flag <- idx[which(df[idx, 1] == "A5D")]
      b5d_flag <- idx[which(df[idx, 1] == "B5D")]
      f5d_flag <- idx[which(df[idx, 1] == "F5D")]
      # Compute inputs and outputs
      input <- is_num(df[a5d_flag, 5]) + is_num(df[b5d_flag, 5]) + is_num(df[f5d_flag, 5])
      output <- is_num(df[b5d_flag, 6]) + is_num(df[f5d_flag, 6])
      # Mark as null entry
      df[idx[1], 1] <- "F5D"
      df[idx[1], 5] <- input
      df[idx[1], 6] <- output
      df[idx[-1], 1] <- "NULL"
      # Dupe index modification
      dupes[idx] <- FALSE
      # Type checker
      done <- TRUE
    }
    
    ### TYPE 4: UNDEFINED ######################################################
    if (done == FALSE) {
      # # Unknown type of repetition
      # print(paste0(file_name, ": Unknown type of repetition"))
      
      browser()
      
      return(NULL)
    }
  }
  return(df)
}

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# Given a file name, return dates, inputs and outputs
foreach (x = 1:length(fnames)) %do% {
  # Read file
  g_df <- data.table::fread(
    file       = paste0(goiener_users_folder, fnames[x]),
    header     = FALSE,
    sep        = ",",
    na.strings = NULL,
    fill       = TRUE,
    select     = 1:12
  )
  # Get file formats
  g_df <- get_file_format(g_df)
  # Manage duplicates!!
  g_df <- manage_duplicates(g_df, fnames[x])
  # If there's no problem with duplicates, keep on formatting
  if (!is.null(g_df)) {
    # Dates in file
    dnv_mat <- t(apply(g_df, 1, extract_data_from_row))
    # Convert matrix to dataframe
    dnv_df <- as.data.frame(dnv_mat)
    # Delete rows with NA
    if (any(is.na(dnv_df$V1))) {
      dnv_df <- dnv_df[-which(is.na(dnv_df$V1)),]
    }
    # Sort dataframe by date
    dnv_df <- dplyr::arrange(dnv_df, V1)
    
    # # Double-check duplications
    # if (any(duplicated(dnv_df$V1))) {
    #   print(paste0(fnames[x], ": There are still repeated dates!"))
    # }
    
    # Convert numbers to dates
    dnv_df$V1 <- as.POSIXct(dnv_df$V1, tz = "UTC", origin = time_origin)
    # Write file
    data.table::fwrite(
      dnv_df,
      file       = paste0(goiener_output_folder, fnames[x]),
      row.names  = FALSE,
      col.names  = FALSE,
      sep        = ",",
      na         = "",
      dateTimeAs = "write.csv"
    )
  } else {
    # Write an entry on the wrong file
    data.table::fwrite(
      data.frame(c(fnames[x])),
      file       = paste0(goiener_output_folder, "wrong_files.csv"),
      row.names  = FALSE,
      col.names  = FALSE,
      sep        = ",",
      na         = "",
      append     = TRUE
    )
  }
}

# Stop parallelization
parallel::stopCluster(cl)
