#' Load Metadata for Indicators
#'
#' This function loads all the metadata of the indicators presented in currently available in the IA package.
#'
#' @return A data frame containing the metadata of indicators, including information such as indicator name, description, data sources, and other relevant attributes.
#' @details The metadata is sourced from the supplementary material from the *IPBES Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature* (Table 1, section "Summary of indicators").
#' @examples
#' # Load the metadata for the indicators
#' indicator_metadata <- load_indicatorSummary()
#' head(indicator_metadata)
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#' @export
#'
load_indicatorSummary <- function() {
  data_path <- system.file("Metadata", "SOD_Table1.csv", package="IA")
  dat<-read.csv(data_path, sep="\t")
  return(dat)
}
