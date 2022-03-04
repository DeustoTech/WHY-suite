# Convert from raw REFIT files (in W) to raw dataframes (in kWh)

# REFIT folder
folder_in  <- "G:/Mi unidad/WHY/Datos (raw)/REFIT/in/"
folder_out <- "G:/Mi unidad/WHY/Datos (raw)/REFIT/out/"
# Get filenames
fnames <- list.files(folder_in)
# Loop
for (fname in fnames) {
  print(fname)
  # Read file
  d_f <- data.table::fread(
    file       = paste(folder_in, fname, sep = ""),
    header     = TRUE,
    sep        = ",",
    na.strings = "",
    select     = c(1,3),
  )              
  # Convert dates
  d_f[[1]] <- as.POSIXct(d_f[[1]], tz = "GMT")
  # Arrange by date
  d_f <- dplyr::arrange(d_f, "Time")
  # Time differences
  time_diffs <- as.numeric(diff(d_f[[1]]))
  # Watt means
  watt_means <- zoo::rollapply(d_f[[2]], width = 2, FUN = mean)
  # Joules
  joules <- watt_means * time_diffs
  # New dataframe
  d_f <- data.frame(times = d_f[-1,1], values = joules)
  # Sum of aggregates
  cut_seq <- cut(d_f[[1]], breaks = "1 hour")
  aggr_sum <- stats::aggregate(
    x   = as.numeric(d_f[[2]]),
    by  = list(date_time = cut_seq),
    FUN = sum
  )
  # New dataframe
  d_f <- data.frame(
    times  = as.POSIXct(aggr_sum[[1]], tz = "GMT"),
    values = as.numeric(aggr_sum[[2]]) / 3.6E6
  )
  # Delete outliers coming from gaps
  time_diff_idx <- which(diff(d_f[[1]]) > 1) + 1
  d_f <- d_f[-time_diff_idx,]
  # Delete first and last (incomplete data)
  d_f <- d_f[-c(1, dim(d_f)[1]), ]
  # Save dataframe
  data.table::fwrite(
    d_f,
    file       = paste(folder_out, fname, sep = ""),
    row.names  = FALSE,
    col.names  = FALSE,
    sep        = ",",
    na         = "",
    dateTimeAs = "write.csv"
  )
}


