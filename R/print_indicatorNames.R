#' Display Names of Indicators
#'
#' This function prints the long names of all indicator data sets available in the IA package
#'
#' @examples
#' print_indicatorNames()
#'
#' @references IPBES (2019). *Global Assessment Chapter 2.2 – Supplementary Material: Indicators of Status & Trends in Nature*.
#' @export
#'
print_indicatorNames <- function() {
  data_path <- system.file("Metadata", "StatusTrendsMetadataFinal.csv", package="IA")
  dat<-read.csv(data_path)
  displayed_dat <- subset(dat, (Do.plots=="Yes" & SupersededBy=="" & File_name != ""))
  print(sort(unique(displayed_dat$Indicator_name)))
}
