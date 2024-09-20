#' Load Indicator Data
#'
#' This function loads indicator data included in the package, located in the `/inst/Global_IA_Data` directory. The data consists of global averages over time. Users can specify the indicator by its full name using the `indicatorName` parameter.
#'
#' @param indicatorName A character string representing the full name of the indicator to be loaded. To view a list of available indicators, use the `print_indicatorNames()` function.
#' @return A data frame containing the year and corresponding global indicator values.
#' @examples
#' # Load data for a specific indicator
#' dat <- load_indicatorData("Aboveground_biomass_PgC.csv")
#' head(dat)
#' plot(dat)
#'
#' @references
#' Data sourced from IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.

load_indicatorData <- function(indicatorName) {
  # Load metadata for the all available indicators in the IA package
  metadata <- load_indicatorMetadata()

  # Check if the provided indicator name is valid (not NA and exists in the metadata)
  if (is.na(indicatorName) || (!(indicatorName %in% metadata$Indicator_name))) {
    # If the indicator name is missing or not found in the metadata, print an error message
    print(
      "Invalid indicator name. Please select a valid name from the available list using print_indicatorNames()."
    )
    # Display the available indicator names for the user
    print_indicatorNames()
  }
  else {
    # Select the file path for the requested indicator from the metadata
    data_path <- system.file("Global_IA_Data", metadata$File_name[metadata$Indicator_name == indicatorName], package = "IA")

    # Read the data from the specified CSV file
    dat <- read.csv(data_path)

    # Subset the data to include the 'year' column and the specific indicator's data column
    dat_subset <- dat[, c(1, metadata$DataColumn[metadata$Indicator_name == indicatorName])]

    # Standardize the column names to 'year' and 'value' for consistency
    names(dat_subset) <- c("year", "value")

    # Return the processed data subset
    return(dat_subset)
  }
}

