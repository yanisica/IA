#' Load Comprehensive Summary of Indicator Metadata
#'
#' This function loads a detailed overview of the metadata for indicators available in the IA package. The summary includes key information such as the indicator name, data sources, time span covered, EBV class, and other relevant attributes.
#'
#' @return A data frame containing comprehensive metadata for the available indicators, including details on the indicator's name, data sources, time span, EBV class, and other relevant attributes.
#' @examples
#' # Load the comprehensive metadata for the indicators
#' Tab1.dat <- load_indicatorSummary()
#' head(Tab1.dat)
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#'
load_indicatorSummary <- function() {
  # Define the file path for the metadata summary
  data_path <- system.file("Metadata", "SOD_Table1.csv", package = "IA")

  # Read the metadata summary file (tab-separated)
  dat <- read.csv(data_path, sep = "\t")

  return(dat)
}
