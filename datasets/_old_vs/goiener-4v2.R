# CONVERT GOIENER USER DATA INTO GOIENER USABLE (TIMESTAMPED) RAW DATASETS
# Improved version that corrects duplicated dates

library(doParallel)

# Path to goiener_users folder
###goiener_users_folder <- "/mnt/disk2/goiener/users-rep/"
goiener_users_folder <- "C:/Users/carlos.quesada/Documents/goiener_users/"
# Output path
###goiener_output_folder <- "/mnt/disk2/goiener/raw-rep/"
goiener_output_folder <- "C:/Users/carlos.quesada/Documents/goiener_users/output/"

# Time origin
time_origin <- as.POSIXct("1970-01-01", tz="UTC")
# File formats
file_formats <- c(
  "P5D", "RF5D", "P4D", "P2D", "P1D", "F5D", "F1", "C2", "C1", "B5D", "A5D")
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
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
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
  dates[date_idx_B] <- as.POSIXct(df$V3, tz="UTC", origin=time_origin) -
    lubridate::hours(df$V4)
  dates[date_idx_C] <- as.POSIXct(df$V4, tz="UTC", origin=time_origin) -
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
    # Check for format = input <- D & output <- E
    if (all(df[idx, 1] == "B5D") | all(df[idx, 1] == "F5D") |
        all(df[idx, 1] == "P5D") | all(df[idx, 1] == "RF5D")) {
      # Check if values are identical
      if (all(compareNA(df[idx, 5], df[[idx[1], 5]])) &
          all(compareNA(df[idx, 6], df[[idx[1], 6]]))) {
        # Mark as null entry
        df[idx[-1], 1] <- "NULL"
        # Dupe index modification
        dupes[idx] <- FALSE
        # Type checker
        done <- TRUE
      }
    }
    # Check for format = input <- E & output <- G
    if (all(df[idx, 1] == "P1D") | all(df[idx, 1] == "P2D")) {
      # Check if values are identical
      if (all(compareNA(df[idx, 6], df[[idx[1], 6]])) &
          all(compareNA(df[idx, 8], df[[idx[1], 8]]))) {
        # Mark as null entry
        df[idx[-1], 1] <- "NULL"
        # Dupe index modification
        dupes[idx] <- FALSE
        # Type checker
        done <- TRUE
      }
    }
    # F1 DOES NOT SEEM TO CORRESPOND TO ANY HOUSEHOLD CONSUMPTION
    # # Check for format = input <- E & output <- F
    # if (all(df[idx, 1] == "F1")) {
    #   # Check if values are identical
    #   if (all(compareNA(df[idx, 6], df[[idx[1], 6]])) &
    #       all(compareNA(df[idx, 7], df[[idx[1], 7]]))) {
    #     # Mark as null entry
    #     df[idx[-1], 1] <- "NULL"
    #     # Dupe index modification
    #     dupes[idx] <- FALSE
    #     # Type checker
    #     done <- TRUE
    #   }
    # }
    
    # Check for format = input <- D
    if (all(df[idx, 1] == "A5D")) {
      # Check if values are identical
      if (all(compareNA(df[idx, 5], df[[idx[1], 5]]))) {
        # Mark as null entry
        df[idx[-1], 1] <- "NULL"
        # Dupe index modification
        dupes[idx] <- FALSE
        # Type checker
        done <- TRUE
      }
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
      input <-
        is_num(df[a5d_flag, 5]) +
        is_num(df[b5d_flag, 5]) +
        is_num(df[f5d_flag, 5])
      output <-
        is_num(df[b5d_flag, 6]) +
        is_num(df[f5d_flag, 6])
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
      # Unknown type of repetition
      print(paste(file_name, ": Unknown type of repetition", sep=""))
      return(NULL)
    }
  }
  return(df)
}

#Setup parallel backend to use many processors
cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

# Given a file name, return dates, inputs and outputs
foreach (x = 1:length(fnames)) %dopar% {
# for (x in 1:length(fnames)) {
  # Print timestamp and current file being analyzed
  print(paste(format(Sys.time(), "%c"), fnames[x], sep=" - "))
  # Read file
  g_df <- data.table::fread(
    file       = paste(goiener_users_folder, fnames[x], sep=""),
    header     = FALSE,
    sep        = ",",
    na.strings = NULL,
    fill       = TRUE
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
    # Double-check duplications
    if (any(duplicated(dnv_df$V1))) {
      print(paste(fnames[x], ": There are still repeated dates!", sep=""))
    }
    # Convert numbers to dates
    dnv_df$V1 <- as.POSIXct(dnv_df$V1, tz = "UTC", origin = time_origin)
    # Write file
    data.table::fwrite(
      dnv_df,
      file       = paste(goiener_output_folder, fnames[x], sep=""),
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
      file       = paste(goiener_output_folder, "+wrong_files.csv", sep=""),
      row.names  = FALSE,
      col.names  = FALSE,
      sep        = ",",
      na         = "",
      append     = TRUE
    )
  }
}

parallel::stopCluster(cl)
