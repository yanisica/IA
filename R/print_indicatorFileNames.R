#' Display Available Indicator File Names
#'
#' This function prints the file names of all available indicator data sets in the IA package. These files are located in the `inst/Global_IA_Data` directory. The function filters out superseded indicators and those without valid file names, displaying only the active and available ones.
#'
#' @return A sorted vector of unique file names for the available indicator data sets.
#' @examples
#' # Example: Print the file names of available indicators
#' print_indicatorFileNames()
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#'
print_indicatorFileNames <- function() {
  # Define the path to the metadata file
  data_path <- system.file("Metadata", "StatusTrendsMetadataFinal.csv", package = "IA")

  # Load the metadata
  dat <- read.csv(data_path)

  # Filter to include only available indicators (valid, not superseded, and eligible for plotting)
  displayed_dat <- subset(dat, Do.plots == "Yes" & SupersededBy == "" & File_name != "")

  # Return a sorted vector of unique file names
  return(sort(unique(displayed_dat$File_name)))
}
