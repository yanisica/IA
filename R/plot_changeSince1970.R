#' Plot Relative Change of Indicator Over Time Since 1970
#'
#' This function generates a plot showing the relative change of a given indicator using 1970 onward. The years are displayed on the x-axis, and the rescaled indicator values (relative to the first available year after 1970) are shown on the y-axis. For details on the rescaling procedure, refer to `rescale_to_1970()`.
#'
#' @param indicatorName A character string representing the full name of the indicator to be plotted. Use `print_indicatorNames()` to view a list of available indicators.
#' @return A ggplot object showing the relative change of the indicator over time. The x-axis represents the year, and the y-axis represents the rescaled values, with the first available year since 1970 set to 100.
#' @examples
#' # Example: Plot the relative change since 1970 for the Aboveground biomass indicator
#' plot_changeSince1970("Aboveground_biomass_PgC.csv")
#'
plot_changeSince1970 <- function(indicatorName) {

  # Load indicator data and metadata
  ts <- load_indicatorData(indicatorName)
  metadata <- subset(load_indicatorMetadata(), Indicator_name == indicatorName)

  # Rescale values relative to 1970
  s70 <- try(rescale_to_1970(v = ts$value, y = ts$year))

  # Check if rescaling was successful and if there are enough values to plot
  if (class(s70) != "try-error" & sum(!is.na(s70)) > 1) {
    biggest <- max(s70$scaled70, na.rm = TRUE)
    ymax <- max(c(biggest, 100))

    # Create the plot for rescaled data with a trend line
    p_scaled <- ggplot2::ggplot(s70, ggplot2::aes(x = year, y = scaled70)) +
      ggplot2::geom_point(colour = "blue", size = 3) +
      ggplot2::xlim(c(1970, 2020)) +
      ggplot2::labs(
        title = metadata$Name_for_plot,
        subtitle = "Change since 1970 or, if later, the first available year",
        y = "Rescaled value (first available year since 1970 = 100)",
        x = "Year"
      )

    # Add confidence intervals if available
    if (sum(!is.na(s70$scaled.upper)) > 1) {
      p_scaled <- p_scaled +
        ggplot2::geom_ribbon(ggplot2::aes(
          ymin = scaled.lower,
          ymax = scaled.upper
        ), fill = "lightgrey") +
        ggplot2::geom_point(colour = "blue", size = 3)
    }

    # Add the main trend line
    p_scaled <- p_scaled + ggplot2::geom_line(ggplot2::aes(x = year, y = scaled.fit), color = "black")

  } else {
    # If not enough data is available, produce a blank plot with an explanatory message
    p_scaled <- ggplot2::ggplot(ts, ggplot2::aes(x = year, y = value)) +
      ggplot2::geom_blank() +
      ggplot2::ylim(c(0, 100)) +
      ggplot2::xlim(c(1970, 2020)) +
      ggplot2::labs(
        title = metadata$Name_for_plot,
        subtitle = "Not enough data to estimate trend since 1970",
        y = "",
        x = ""
      )
  }

  return(p_scaled)
}
