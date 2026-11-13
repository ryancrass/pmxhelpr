#' Time-axis breaks on natural time steps
#'
#' @description
#' `breaks_time()` returns a breaks function for a continuous time axis that
#' places ticks on steps natural to the time unit, e.g. every 4, 12, or 24
#' hours, or every 7 or 28 days, instead of ggplot2's default multiples of 1,
#' 2, 2.5, or 5 (0, 50, 100, 150 hours).
#'
#' The time-axis plots ([plot_dvtime()], [plot_gof()], [plot_vpc_cont()],
#' [plot_vpc_cens()]) use it by default through their `time_unit` argument.
#' Pass it to [ggplot2::scale_x_continuous()] for other plots.
#'
#' @param unit Time unit of the axis: `"hours"` (default), `"days"`,
#'    `"weeks"`, or `"months"`. Short forms are accepted: `"h"`, `"hr"`,
#'    `"d"`, `"w"`, `"wk"`, `"m"`, and `"mo"`, among others.
#' @param n Maximum number of breaks. Default is 8.
#'
#' @details
#' The breaks use the smallest step for the unit that gives at most `n`
#' breaks within the axis limits:
#' * hours: 0.5, 1, 2, 4, 6, 8, 12, 24, 48, 72, 168 (1 week), 336, 672
#'   (4 weeks)
#' * days: 1, 2, 7, 14, 28, 56, 84, 168, 364
#' * weeks: 1, 2, 4, 8, 12, 26, 52
#' * months: 1, 2, 3, 6, 12, 24
#'
#' A span too long for the largest step uses a multiple of it. Breaks are
#' multiples of the step, so negative (pre-dose) times are handled.
#'
#' @return A function of the axis limits that returns a numeric vector of
#'    breaks, for the `breaks` argument of a continuous scale.
#' @export
#'
#' @examples
#' breaks_time()(c(0, 168))
#' breaks_time("days")(c(0, 28))
#' breaks_time("hours", n = 5)(c(-1, 24))
#'
#' ggplot2::ggplot(data_sad, ggplot2::aes(x = TIME, y = ODV)) +
#'   ggplot2::geom_point() +
#'   ggplot2::scale_x_continuous(breaks = breaks_time("hours"))

breaks_time <- function(unit = "hours", n = 8) {
  unit <- normalize_time_unit(unit)
  check_positive_integer(n, "n")
  if (n < 2) pmx_abort("{.arg n} must be at least 2.", "bad_arg")
  steps <- .time_steps[[unit]]
  function(limits) time_breaks(limits, steps, n)
}


.time_steps <- list(
  hours  = c(0.5, 1, 2, 4, 6, 8, 12, 24, 48, 72, 168, 336, 672),
  days   = c(1, 2, 7, 14, 28, 56, 84, 168, 364),
  weeks  = c(1, 2, 4, 8, 12, 26, 52),
  months = c(1, 2, 3, 6, 12, 24)
)


## Breaks on multiples of the smallest step giving at most `n` breaks within
## `limits`; past the largest step, the smallest multiple of it that fits.
time_breaks <- function(limits, steps, n) {
  limits <- limits[is.finite(limits)]
  if (length(limits) == 0) return(numeric(0))
  lo <- min(limits)
  hi <- max(limits)
  if (lo == hi) return(lo)
  n_breaks <- function(step) floor(hi / step) - ceiling(lo / step) + 1
  fits <- steps[n_breaks(steps) <= n]
  step <- if (length(fits) > 0) {
    fits[1]
  } else {
    largest <- steps[length(steps)]
    k <- ceiling((hi - lo) / ((n - 1) * largest))
    while (n_breaks(k * largest) > n) k <- k + 1
    k * largest
  }
  seq(ceiling(lo / step), floor(hi / step)) * step
}


normalize_time_unit <- function(unit, arg = rlang::caller_arg(unit),
                                call = rlang::caller_env()) {
  aliases <- c(
    hours  = "hours",  hour  = "hours",  hrs = "hours", hr = "hours", h = "hours",
    days   = "days",   day   = "days",   d   = "days",
    weeks  = "weeks",  week  = "weeks",  wks = "weeks", wk = "weeks", w = "weeks",
    months = "months", month = "months", mos = "months", mo = "months", m = "months"
  )
  if (!is.character(unit) || length(unit) != 1L || is.na(aliases[unit])) {
    pmx_abort(c("{.arg {arg}} must be one of {.or {.val {c('hours', 'days', 'weeks', 'months')}}}.",
                "i" = "Short forms such as {.val h}, {.val d}, {.val wk}, and {.val mo} also work."),
              "bad_arg", call = call)
  }
  unname(aliases[unit])
}


## Time-aware x breaks for a time-axis plot, unless the style sets `xbreaks`
## or `logx`. With no other x-scale fields in the style, the breaks function
## goes on the plot's x scale, so it follows each panel's range and any zoom.
## A style that sets `xlims`, `xminor_breaks`, or `xtick_labels` makes
## style_plot() build the x scale itself, so the breaks are computed for
## `xlims` (or the plot's x range) and written to the style instead.
style_time_axis <- function(plot, style, time_unit) {
  if (!is.null(style$xbreaks) || isTRUE(style$logx)) {
    return(list(plot = plot, style = style))
  }
  breaks <- breaks_time(time_unit)
  if (is.null(style$xlims) && is.null(style$xminor_breaks) && is.null(style$xtick_labels)) {
    plot <- plot + ggplot2::scale_x_continuous(breaks = breaks)
  } else {
    style$xbreaks <- breaks(style$xlims %||% ggplot2::layer_scales(plot)$x$range$range)
  }
  list(plot = plot, style = style)
}
