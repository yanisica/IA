#' Plot Absolute Values of Indicator Over Time
#'
#' This function generates a plot showing the absolute values of a given indicator over time, with years on the x-axis and raw indicator values on the y-axis.
#'
#' @param indicatorName A character string representing the full name of the indicator to be plotted. To view a list of available indicators, use the `print_indicatorNames()` function.
#'
#' @return A ggplot figure with "Year" on the x-axis and "Raw value" on the y-axis, representing the absolute values of the selected indicator.
#' @examples
#' # Example: Plot the absolute values of the Aboveground biomass indicator
#' plot_absoluteValue("Aboveground_biomass_PgC.csv")
#'
plot_absoluteValue <- function(indicatorName) {
  # Load indicator data using the IA package's internal function
  ts <- load_indicatorData(indicatorName)

  # Create the plot with ggplot2
  p_absolute <- ggplot2::ggplot(ts, ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_point(colour = "red", size = 3) +
    ggplot2::labs(
      title = indicatorName,
      x = "Year",
      y = "Raw value",
      subtitle = "Expressed on the scale of raw values"
    )

  return(p_absolute)
}
