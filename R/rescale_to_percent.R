#' Rescale an Indicator to Percentage
#'
#' This function rescales an indicator to a percentage scale for indicators where the values of an intact (pristine) world and a fully-degraded world can be specified.
#' It calculates how far each time point is from pristine (100%) to fully degraded (0%).
#' If the indicator is not suitable for rescaling, the function will return a vector of NAs of the same length as the indicator.
#'
#' @param v A numeric vector representing the raw values of the indicator.
#' @param y A numeric vector representing the years corresponding to the indicator values.
#' @param hl A string indicating whether "High" or "Low" values are better ("High" for indicators where higher values are better, "Low" for indicators where lower values are better).
#' @param pv Numeric value representing the pristine value of the indicator (the value in an intact world).
#' @param agv Numeric value representing the all-gone value of the indicator (the value in a fully degraded world).
#' @param gt An integer threshold for the number of data points required to use a generalized additive model (GAM). The default is 5.
#'
#' @return A data frame with the original values, rescaled percentages, and fitted percentages over time.
#'
#' @examples
#' # Example usage:
#' # rescale_to_percent(v = indicator_values, y = years, hl = "High", pv = pristine_value, agv = all_gone_value)
#'
rescale_to_percent <- function(v, y, hl, pv, agv, gt = 5) {

  # Initialize a data frame with NA values in case the indicator cannot be meaningfully rescaled
  nas <- rep(NA, length(v))
  percv <- data.frame(year = y, raw = v, percent = nas, percent.fit = nas)

  # Check if pristine and all-gone values are available for rescaling
  if (!is.na(pv) & !is.na(agv)) {
    # Rescale based on whether "High" or "Low" values are better
    if (hl == "High") {
      percv$percent <- 100 * (percv$raw - agv) / (pv - agv)
    }
    if (hl == "Low") {
      percv$percent <- 100 * (percv$raw - pv) / (agv - pv)
    }
  }

  # Filter for values starting from 1970
  percv$p70 <- c(rep(NA, sum(percv$year < 1970)), percv$percent[percv$year >= 1970])

  # If there are fewer than 3 values after 1970, use the available values as the fitted values
  if (sum(!is.na(percv$p70)) < 3) {
    percv$percent.fit <- percv$p70
    return(percv)
  }

  # If there are enough values for linear regression but fewer than the GAM threshold, apply linear regression
  if (sum(!is.na(percv$p70)) >= 3 & sum(!is.na(percv$p70)) < gt) {
    m1 <- lm(p70 ~ year, data = percv)
    fits <- predict(m1, se = FALSE)
    percv$percent.fit <- c(rep(NA, sum(percv$year < 1970)), fits)
    return(percv)
  }

  # If there are enough values for a GAM, apply it with flexibility on the basis dimension
  if (sum(!is.na(percv$p70)) >= gt) {
    if (sum(!is.na(percv$p70)) > 10) {
      m2 <- mgcv::gam(p70 ~ s(year), gamma = 1.4, data = percv)  # Use default basis dimension
    } else {
      m2 <- mgcv::gam(p70 ~ s(year, k = 3), gamma = 1.4, data = percv)  # Use k = 3 for small datasets
    }
    gamfits <- predict(m2, se = FALSE, type = "response")
    percv$percent.fit <- c(rep(NA, sum(percv$year < 1970)), gamfits)
    return(percv)
  }
}
