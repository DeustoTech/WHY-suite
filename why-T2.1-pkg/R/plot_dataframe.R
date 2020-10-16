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
    coloring[dset_data[,3] == 2] = "darkgreen" #"green"
  } else {
    coloring = "blue"
  }

  p <- ggplot2::ggplot(
    # Data to be plotted
    data = dset_data,
    mapping = ggplot2::aes(x=times, y=values)
    ) +
    # Type of graph (line in this case)
    ggplot2::geom_line(
      color = coloring
    ) +
    # Title
    ggplot2::ggtitle(
      title
    ) +
    # Labels
    ggplot2::labs(
      x = "Date",
      y = "kWh"
    ) + 
    # Axis limits
    ggplot2::scale_y_continuous(limits = c(0,5))

  print(p)

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
