#' Determine axis breaks automatically for time variables
#'
#' @param x Numeric vector of times from which to determine breaks
#' @param unit Character string for time units.
#'    Options include:
#'    + "hours" (default), "hrs", "hour", "hr", "h"
#'    + "days", "dys", "day", "dy", "d"
#'    + "weeks", "wks", "week", "wk", "w"
#'    + "months", "mons", "mos", "month", "mo", "m"
#'
#' @param n Ideal number of axis breaks requested (default = 8). Passed to `labeling::extended()`
#'
#' @return A numeric vector of breaks
#' @keywords internal
#' @examples
#' ntimes <- sort(unique(data_sad$NTIME))
#' breaks <- pmxhelpr:::var_timebreaks(ntimes)

var_timebreaks <- function(x, unit = "hours", n = 8) {
  check_numeric(x, "x")
  check_integer(n, "n")

  unit <- normalize_time_unit(unit, "unit")
  x <- as.numeric(x)
  if (length(x) == 0L || all(is.na(x))) {
    rlang::abort("argument `x` must contain at least one non-NA numeric value")
  }
  rng <- range(x, na.rm = TRUE)

  scale <- switch(unit,
                  hours  = 24,
                  days   = 7,
                  weeks  = 1,
                  months = 1)
  rng <- rng / scale

  Ql <- if (max(rng, na.rm = TRUE) <= 1) {
    switch(unit,
           hours  = c(4/24, 8/24, 12/24, 1),
           days   = c(1/7, 1),
           weeks  = c(0.5, 1),
           months = c(0.5, 1))
  } else {
    c(1, 2, 4, 7)
  }

  breaks <- labeling::extended(rng[1], rng[2], n,
                               Q = Ql, only.loose = FALSE) * scale
  breaks[breaks <= max(x, na.rm = TRUE)]
}
