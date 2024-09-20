#' Load Indicator Metadata (Internal Use)
#'
#' This function retrieves and returns the metadata for all indicator data sets included in the IA package. The metadata provides detailed information such as indicator names, associated data file names, and instructions for plotting and data transformation. Note that this function is intended for internal package use only. For a more user-friendly and structured overview of the metadata, use `load_indicatorSummary()`.
#'
#' @return A data frame containing metadata for indicators that are available and verified.
#' @examples
#' load_indicatorMetadata()
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.


load_indicatorMetadata <- function() {
  # Define the path to the metadata file within the package
  data_path <- system.file("Metadata", "StatusTrendsMetadataFinal.csv", package = "IA")

  # Read the metadata CSV file
  dat <- read.csv(data_path)

  # Filter to return only the metadata for indicators marked as "available" (i.e., not superseded, with valid file names)
  displayed_dat <- subset(dat, (Do.plots == "Yes" & SupersededBy == "" & File_name != ""))

  return(displayed_dat)
}
