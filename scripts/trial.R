extend_datasets <- function(input_folder, output_folder) {
  # Path to ACORN folder
  acorn_folder <- paste("G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/", 
                        "informations_households.csv", sep="")
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder)
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset and impute
    print(dset_filename)
    file_path <- paste(input_folder, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    cdf <- cook_raw_dataframe(raw_df     = rdf, 
                              from_date  = "first", 
                              to_date    = "last", 
                              dset_key   = "lcl", 
                              filename   = dset_filename, 
                              acorn_path = acorn_folder)
    # Get length
    initial_date   <- cdf$df[1,1]
    final_date     <- tail(cdf$df, n=1)[[1]]
    length_in_days <- as.numeric(final_date - initial_date)
    # If TS is longer than 365 days, impute; ELSE discard
    if (length_in_days > 365) {
      idf <- impute_cooked_dataframe(
        cdf       = cdf, 
        season    = cdf$seasonal_periods[1] * 7, 
        short_gap = cdf$seasonal_periods[1] / 3
      )
      # Expand if needed
      edf <- extend_imputed_dataframe(idf=idf, wanted_days=800)
      # Save dataframe in output folder
      path <- paste(output_folder, substr(dset_filename,1,9), sep="")
      save(edf, file=path)
      print("SAVED!")
    }
    else print("DISCARDED")
  }
}

extend_imputed_dataframe <- function(idf, wanted_days) {
  # Get current length in months of idf
  idf_init_date  <- idf$df[1,1]
  idf_final_date <- tail(idf$df, n=1)[[1]]
  idf_final_wday <- lubridate::wday(idf_final_date)
  idf_days       <- as.numeric(idf_final_date - idf_init_date)

  # Length of the extension in days
  extens_in_days <- ceiling(wanted_days - idf_days)
  
  ### OJO - SUPONIENDO QUE ESTA EXTENSION ES MENOR QUE 365 DIAS
  
  # If idf is longer than required, do nothing!
  if (extens_in_days > 0) {
    ## Look for the exact point of extraction than matches the weekday
    # Subtract one year
    extr_init_date <- idf_final_date - lubridate::days(365)
    # Check weekdays: if original ends on Monday, copy must start on Monday
    extr_init_date_wday <- lubridate::wday(extr_init_date)
    # Get the shortest path of two 
    diff_wdays <- c(
      (idf_final_wday - extr_init_date_wday) %% 7,
      - ((extr_init_date_wday - idf_final_wday) %% 7)
      )
    # Get the absolute minimum
    diff_wdays <- diff_wdays[which.min(abs(diff_wdays))][1]
    # Extraction interval
    extr_init_date <- extr_init_date + lubridate::days(diff_wdays)
    extr_final_date <- extr_init_date + lubridate::days(extens_in_days)
    # Extraction indices
    extr_idx <- idf$df[,1] > extr_init_date & idf$df[,1] < extr_final_date
    browser()
    # Extracted times and values
    extr_times  <- idf$df[extr_idx, 1]
    extr_values <- idf$df[extr_idx, 2]
    # Extended times
    extend_times <- seq(
      from       = idf_final_date,
      by         = as.difftime(86400/idf$seasonal_periods[1], units="secs"),
      length.out = length(extr_times) + 1
    )
    # Create the extended dataframe
    extend_df <- data.frame(
      times          = extend_times[2:length(extend_times)],
      values         = extr_values,
      imputed        = 2,
      original_times = extr_times
    )
    # Bind rows
    idf$df            <- rbind(idf$df, extend_df)
    idf$number_of_ext <- length(extr_times)
    if (extens_in_days > 730) {
      low_seas <- idf$seasonal_periods[1]
      idf$seasonal_periods <- c(low_seas, low_seas*7, low_seas*365)
    }
  }
  return(idf)
}

# User parameters
input_folder  <- "G:/Mi unidad/WHY/Datasets/lcl/"
output_folder <- "G:/Mi unidad/WHY/Datasets/lcl-ext2/"

# Function call
extend_datasets(input_folder, output_folder)