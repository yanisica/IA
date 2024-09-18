



#' Load Annual Global Data for Indicators
#'
#' This function loads a the global annual indicator data called by its name
#'
#' @return A data frame containing year and global indicator value
#' @details The data is sourced from the supplementary material from the *IPBES Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#' @examples
#' # Load the metadata for the indicators
#' dat <- load_indicatorData("Aboveground_biomass_PgC.csv")
#' head(dat)
#' plot(dat)
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#' @export
#'
load_indicatorData <- function(dataset.name) {
  data_path <- system.file("Global_IA_Data", dataset.name, package="IA")
  dat<-read.csv(data_path)
  return(dat)
}

