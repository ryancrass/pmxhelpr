#' Determine axis breaks automatically for time variables
#'
#' @param x Numeric vector of times from which to determine breaks
#' @param unit Character string for time units.
#'    Options include:
#'    + "hours" (default)
#'    + "days"
#'    + "weeks"
#'    + "months"
#' @param n Ideal number of axis breaks requested (default = 8). Passed to `labeling::extended()`
#'
#' @return A numeric vector of breaks
#'
#' @export breaks_time
#'
#' @examples
#'ntimes <- sort(unique(data_sad$NTIME))
#'breaks <- breaks_time(ntimes)
#'
#'
breaks_time <- function(x, unit="hours", n=8) {

  #Checks
  check_numeric(x)
  check_timeu(unit)
  check_integer(n)

  x <- as.numeric(x)

  #Define range
  rng <- range(x, na.rm = TRUE)

  if (unit == "hours") {
    scale <- 24
  } else if (unit == "days") {
    scale <- 7
  } else if (unit == "weeks") {
    scale <- 1
  } else if (unit == "months") {
    scale <- 1
  }

  rng <- rng / scale

  if(max(rng, na.rm = TRUE)<=1) {
    if(unit == "hours") Ql <- c(4/24, 8/24, 12/24, 1)
    if(unit == "days") Ql <- c(1/7, 1)
    if(unit %in% c("weeks", "months")) Ql <- c(0.5, 1)

    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = Ql,
      only.loose = FALSE)*scale

    breaks <- breaks[breaks<=max(x, na.rm=TRUE)]
  } else {
    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = c(1, 2, 4, 7),
      only.loose = FALSE)*scale

    breaks <- breaks[breaks<=max(x, na.rm = TRUE)]
  }

  return(breaks)
}
