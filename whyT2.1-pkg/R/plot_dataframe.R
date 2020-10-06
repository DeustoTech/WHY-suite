#' Plotting of a dataframe
#'
#' @description
#' Plot a dataframe (either raw or cooked).
#'
#' @param dset_data The dataframe to be plotted.
#' @param title Optional title (`NULL` by default).
#'
#' @return Plot of the dataset.
#'
#' @export

plot_dataframe <- function(dset_data, title=NULL) {
  # Create plot
  p <- plot(
    x    = dset_data[[1]],
    y    = dset_data[[2]],
    col  = "blue",
    type = "l",
    main = title,
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
}
