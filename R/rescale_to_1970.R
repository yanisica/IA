#' Rescale Indicator Values Since 1970
#'
#' This function trims off data prior to 1970 and rescales the remaining values as a percentage of the first available value after 1970.
#' It fits a line or curve to the data and uses the fitted value from the first year for rescaling, which is more reliable for noisy or fluctuating time series.
#'
#' - For n = 2, a straight line is used with no standard errors (SEs).
#' - For 2 < n < gt, a linear regression is used to estimate fits, and SEs are returned with the fits.
#' - For n >= gt, a generalized additive model (GAM) is used. If n <= 10, the basis dimension `k` is set to 3; otherwise, the default is used. SEs are returned with the fits.
#' - For indicators where "High is better", the all-gone value is needed. For "Low is better", the pristine value is required.
#'
#' @param v Numeric vector representing the indicator values.
#' @param y Numeric vector representing the years associated with the indicator values.
#' @param gt Integer threshold for determining whether to use a GAM or linear regression for fitting (default is 5).
#'
#' @return A data frame with rescaled values, including the original data, fitted values, SEs, and the rescaled values relative to 1970.
#'
rescale_to_1970 <- function(v, y, gt = 5) {

  # Filter data for years since 1970
  v <- v[y >= 1970]
  y <- y[y >= 1970]

  # Initialize a data frame for rescaled values, starting with NA in all columns
  nas <- rep(NA, length(v))
  rv <- data.frame(
    year = y[y >= 1970],
    raw = nas,
    fit = nas,
    se = nas,
    scaled70 = nas,
    scaled.fit = nas,
    scaled.upper = nas,
    scaled.lower = nas
  )


  # Return NAs if there are fewer than 2 valid data points
  if (sum(!is.na(v)) < 2) {
    return(rv)
  }

  # If there are exactly 2 valid points, rescale based on the first point
  if (sum(!is.na(v)) == 2) {
    rv$raw <- v
    rv$fit <- v
    rv$scaled70 <- 100 * rv$fit / rv$fit[1]
    rv$scaled.fit <- rv$scaled70
    return(rv)
  }

  # If there are enough points for linear regression but fewer than the threshold (gt), apply a linear model
  if (sum(!is.na(v)) < gt) {
    rv$raw <- v
    m1 <- lm(raw ~ year, data = rv)
    fits <- predict(m1, se = TRUE)
    rv$fit <- fits$fit
    rv$se <- fits$se
    rv$scaled70 <- 100 * rv$raw / rv$fit[1]
    rv$scaled.fit <- 100 * rv$fit / rv$fit[1]
    rv$scaled.upper <- 100 * (rv$fit + 1.96 * rv$se) / rv$fit[1]
    rv$scaled.lower <- 100 * (rv$fit - 1.96 * rv$se) / rv$fit[1]
    return(rv)
  }

  # If there are enough points for a GAM, apply it with flexibility on the basis dimension
  if (sum(!is.na(v)) >= gt) {
    rv$raw <- v
    if (sum(!is.na(v)) > 10) {
      m2 <- mgcv::gam(raw ~ s(year), gamma = 1.4, data = rv)  # Use default basis dimension
    } else {
      m2 <- mgcv::gam(raw ~ s(year, k = 3), gamma = 1.4, data = rv)  # Use k = 3 for small datasets
    }
    gamfits <- predict(m2, se = TRUE, type = "response")
    rv$fit <- gamfits$fit
    rv$se <- gamfits$se
    rv$scaled70 <- 100 * rv$raw / rv$fit[1]
    rv$scaled.fit <- 100 * rv$fit / rv$fit[1]
    rv$scaled.upper <- 100 * (rv$fit + 1.96 * rv$se) / rv$fit[1]
    rv$scaled.lower <- 100 * (rv$fit - 1.96 * rv$se) / rv$fit[1]
    return(rv)
  }
}
