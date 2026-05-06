#' Internal helper: Compute error bar width
#'
#' Returns the user-specified error bar width from the theme, or defaults
#' to 2.5 percent of the maximum `NTIME` value.
#'
#' @param plottheme Named list of theme elements containing `$cent_errorbar$width`.
#' @param data data.frame containing `NTIME` column.
#'
#' @return Numeric error bar width value
#' @keywords internal
#' @examples
#' theme <- plot_dvtime_theme()
#' data <- data.frame(NTIME = c(0, 1, 2, 4, 8, 24))
#' pmxhelpr:::errorbar_width(theme, data)
#'
errorbar_width <- function(plottheme, data) {
  if(is.numeric(plottheme$cent_errorbar$width)) return(plottheme$cent_errorbar$width)
  if (!"NTIME" %in% colnames(data) || nrow(data) == 0L || all(is.na(data$NTIME))) {
    rlang::warn("cannot compute default errorbar width: `NTIME` is empty or all NA. Returning NA; set `width` via `pmx_errorbar(width = ...)` to suppress this warning.")
    return(NA_real_)
  }
  max(data$NTIME, na.rm = TRUE) * 0.025
}


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
  check_timeu(unit)
  check_integer(n, "n")

  x <- as.numeric(x)
  if (length(x) == 0L || all(is.na(x))) {
    rlang::abort("argument `x` must contain at least one non-NA numeric value")
  }
  rng <- range(x, na.rm = TRUE)

  if (unit %in% c("hours", "hrs", "hour", "hr", "h")) {
    scale <- 24
  } else if (unit %in% c("days", "dys", "day", "dy", "d")) {
    scale <- 7
  } else if (unit %in% c("weeks", "wks", "week", "wk", "w")) {
    scale <- 1
  } else if (unit %in% c("months", "mons", "mos", "month", "mo", "m")) {
    scale <- 1
  }

  rng <- rng / scale

  if(max(rng, na.rm = TRUE) <= 1) {
    if(unit %in% c("hours", "hrs", "hour", "hr", "h")) Ql <- c(4/24, 8/24, 12/24, 1)
    if(unit %in% c("days", "dys", "day", "dy", "d")) Ql <- c(1/7, 1)
    if(unit %in% c("weeks", "wks", "week", "wk", "w",
                   "months", "mons", "mos", "month", "mo", "m")) Ql <- c(0.5, 1)

    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = Ql,
      only.loose = FALSE) * scale

    breaks <- breaks[breaks <= max(x, na.rm = TRUE)]
  } else {
    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = c(1, 2, 4, 7),
      only.loose = FALSE) * scale

    breaks <- breaks[breaks <= max(x, na.rm = TRUE)]
  }

  breaks
}
