
#' Print Absolute values of Indicator over Time
#'
#' @param The filename of the indicator that should be displayed
#'
#' @return figure plot with x-axes "year"  and y-axes "raw data"
#' @export
#' @examples
#' plot_absoluteValue("Aboveground_biomass_PgC.csv")
#' @importFrom ggplot2 ggplot aes geom_point
#'
#'
plot_absoluteValue<-function(indicatorFileName){
  if (is.na(indicatorFileName) || (!(indicatorFileName %in% IA::print_indicatorFileNames()))){
    print("Please select a file name from the avaiable indicator list. Use print_indicatorFileName() for an overview.")
  }
  else{
  ts<-load_indicatorData(indicatorFileName)
  names(ts) <- c("year", "value")
#Produce the plot of the raw data for the Appendix
p_absolute <- ggplot2::ggplot(ts, ggplot2::aes(x=year, y=value, show.legend=FALSE)) +
  ggplot2::geom_point(colour = "red", size = 3) +
  ggplot2::labs(title=indicatorFileName,
       y="Raw value",
       x="Year",
       subtitle=("Expressed on the scale of the raw values"))

return(p_absolute)
}
}
