#' Load a time series
#'
#' ---
#' @param dataset_path ---
#' @param filename ---
#' @keywords
#' @export
#' @examples
#' load_ts()

load_ts<- function(dataset_path, filename) {
  library(data.table)
  # Load data from CSV file
  data <- fread(#read.table(
    file = paste(dataset_path, filename, sep = ""),
    header = FALSE,
    sep = ","
  )
  # Times
  times <- ymd_hms(data$V1)
  # Values
  values <- data$V2
  # Create tibble
  data.frame(times = times, values = values)
}
