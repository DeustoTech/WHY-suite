#' Plotting of a row of `data_info` plotting
#'
#' @description
#' Plot the dataset indicated in a row of the file `data_info.csv`.
#'
#' @param di_row Row of `data_info`.
#' @param dset_key Key of the dataset.
#'
#' @return Plot of the dataset.
#'
#' @examples
#' \dontrun{
#' plot_datainfo_row(data_info["75220",])
#' }
#'
#' @export

plot_datainfo_row  <- function(di_row, dset_key="lcl") {
  # How long a step is in seconds
  step_in_secs <- 86400 / SAMPLES_PER_DAY[[dset_key]] # 30 * 60
  # Load a complete time series from the dataset
  csv_path <- paste(DATASET_PATH, di_row[[1]], sep="")
  raw_df   <- get_raw_dataframe_from_dataset(csv_path)
  # Dates
  from_date <- as.POSIXct(di_row[[2]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  to_date   <- as.POSIXct(di_row[[3]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  # Interval
  cooked_df <- cook_raw_dataframe(
    raw_df    = raw_df,
    from_date = from_date,
    to_date   = to_date,
    dset_key  = dset_key
  )
  # Plot
  plot(
    seq(from_date, to_date, as.difftime(step_in_secs, units = "secs")),
    cooked_df,
    type = "l",
    main = di_row[[1]],
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
}
