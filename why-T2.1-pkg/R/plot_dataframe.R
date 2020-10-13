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

  # Check existence of a 3rd column to apply color
  if (dim(dset_data)[2] > 2) {
    # Color vector is blue by default
    coloring = rep("blue", dim(dset_data)[1])
    # Replace ex-NA by red
    coloring[dset_data[,3] == 1] = "red"
    coloring[dset_data[,3] == 2] = "green"
  } else {
    coloring = "blue"
  }

  plot1 <- ggplot(
    data = dset_data,
    mapping = aes(x=times, y=values)
    ) +
    geom_line(
      color = coloring
    ) +
    ggtitle(title)

  print(plot1)

  ## Create plot
  # p <- plot(
  #   x    = dset_data[[1]],
  #   y    = dset_data[[2]],
  #   col  = "blue",
  #   type = "l",
  #   main = title,
  #   xlab = "Date",
  #   ylab = "kWh",
  #   ylim = c(0,5)
  # )
}
