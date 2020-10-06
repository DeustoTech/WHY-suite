#' Raw dataframe from dataset
#'
#' @description
#' Get a dataframe from a dataset contained in a CSV file.
#'
#' @param csv_file String with the absolute path to the CSV file containing the dataset.
#'
#' @return Raw dataframe, i.e. without checking missing samples.
#'
#' @export

get_raw_dataframe_from_dataset <- function(csv_file) {
  # Load data from CSV file
  data <- data.table::fread(
    file = csv_file,
    header = FALSE,
    sep = ","
  )
  # Times
  times <- as.POSIXct(data$V1, tz="GMT")
  # Values
  values <- data$V2
  # Create dataframe
  return(data.frame(times = times, values = values))
}
