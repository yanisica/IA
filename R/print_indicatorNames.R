#' Display Long Names of Available Indicators
#'
#' This function prints the long names of all available indicator data sets included in the IA package.
#'
#' @return A sorted list of unique long names of available indicators.
#' @examples
#' # Example: Print the long names of available indicators
#' print_indicatorNames()
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#'
print_indicatorNames <- function() {
  # Define the path to the metadata file
  data_path <- system.file("Metadata", "StatusTrendsMetadataFinal.csv", package = "IA")

  # Load the metadata
  dat <- read.csv(data_path)

  # Filter to include only indicators that are available (valid, not superseded, and eligible for plotting)
  displayed_dat <- subset(dat, Do.plots == "Yes" & SupersededBy == "" & File_name != "")

  # Print the sorted list of unique indicator names
  print(sort(unique(displayed_dat$Indicator_name)))
}
