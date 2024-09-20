#' Plot Percentage Remaining of the Indicator Over Time
#'
#' This function generates a trend line plot for a given indicator over time. If applicable, the data is rescaled to a percentage scale where 100 represents the "pristine" state and 0 represents complete depletion. For details on the calculation see the IA function `rescale_to_percent()`
#'
#' @param indicatorName A character string representing the full name of the indicator to be plotted. Use `print_indicatorNames()` to view a list of available indicators.
#'
#' @return A ggplot object displaying the trend line with the x-axis representing "Year" and the y-axis representing either the raw or rescaled values.
#' @examples
#' # Example: Plot the trend line for the Percentage of Natural Habitat Extent
#' plot_percentageRemaining("Percentage natural habitat extent")

plot_percentageRemaining <- function(indicatorName) {

  # Load indicator data and corresponding metadata
  ts <- load_indicatorData(indicatorName)
  metadata <- subset(load_indicatorMetadata(), Indicator_name == indicatorName)


  # Attempt to rescale the data to a percentage (if possible)
  pct <- try(rescale_to_percent(
    v = ts$value,
    y = ts$year,
    hl = metadata$HiOrLoBetter,
    pv = metadata$PristineValue,
    agv = metadata$AllGoneValue
  ))

  # If rescaling is successful and there are valid values, generate the plot
  if (class(pct) != "try-error" & sum(!is.na(pct$percent)) > 0) {
    biggest <- max(pct$percent, na.rm = TRUE)
    ymax <- max(c(biggest, 100))

    # Plot the rescaled data on a percentage scale
    p_percent <- ggplot2::ggplot(pct, ggplot2::aes(x = year, y = percent)) +
      ggplot2::geom_point(colour = "purple", size = 3) +
      ggplot2::geom_line(ggplot2::aes(y = percent.fit), color = "black") +
      ggplot2::ylim(c(0, ymax)) +
      ggplot2::labs(
        title = metadata$Name_for_plot,
        subtitle = "Scale: 0 = nothing left, 100 = pristine state",
        y = "Rescaled value (pre-driver state = 100)",
        x = "Year"
      )
  } else {
    # If rescaling is not possible, generate a blank plot with a relevant message
    ts$percent <- NA
    p_percent <- ggplot2::ggplot(ts, ggplot2::aes(x = year, y = percent)) +
      ggplot2::geom_blank() +
      ggplot2::labs(
        title = metadata$Name_for_plot,
        subtitle = "Cannot rescale to pristine-to-destroyed scale",
        y = "",
        x = "Year"
      )
  }

  return(p_percent)
}
