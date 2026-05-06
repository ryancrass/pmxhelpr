#' Test whether an object is a `doseprop_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"doseprop_stats"` class
#' -- i.e., it is the container returned by [df_doseprop()] (also accepted by
#' [plot_doseprop()] and [plot_build_doseprop()] on the precomputed-stats
#' fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    `validate_doseprop_stats()` and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @return Logical scalar.
#' @export is_doseprop_stats
#'
#' @examples
#' stats <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
#' is_doseprop_stats(stats)             # TRUE
#' is_doseprop_stats(as.data.frame(stats))  # FALSE -- coerced to plain frame

is_doseprop_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "doseprop_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_doseprop_stats(x); TRUE}, error = function(e) FALSE)
}



#' Print method for `doseprop_stats`
#'
#' @description
#' Focused summary of a [df_doseprop()] result: object dimensions, the
#' regression configuration values (`metric_var`, `exp_var`, `dose_var`,
#' `ci`, `method`), the number of observation rows attached for the plot
#' scatter overlay, and the per-metric stats body. Inspect the underlying
#' frames directly via `x$stats` and `x$obs`; inspect run config via
#' `x$config`.
#'
#' @param x A `doseprop_stats` object.
#' @param ... Passed to the underlying `print.data.frame()` call for the body.
#'
#' @return `invisible(x)`.
#' @export
#' @method print doseprop_stats

print.doseprop_stats <- function(x, ...) {
  cat("<doseprop_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(x$stats), ncol(x$stats)))
  cat(sprintf("  obs:   %d rows\n",
              if (is.data.frame(x$obs)) nrow(x$obs) else 0L))
  cat(sprintf("  config: metric_var = %s, exp_var = %s, dose_var = %s, ci = %s, method = %s\n",
              format(x$config$metric_var),
              format(x$config$exp_var),
              format(x$config$dose_var),
              format(x$config$ci),
              format(x$config$method)))
  cat("\n  stats body:\n")
  print(x$stats, ...)
  cat("\n  Use `x$obs` for the observation overlay.\n")
  invisible(x)
}



#' Summary method for `doseprop_stats`
#'
#' @description
#' Compact summary of a [df_doseprop()] result: the same header and
#' configuration values shown by [print.doseprop_stats()], but the body is
#' condensed to one line per metric using the `PowerCI` and `Interpretation`
#' columns. Suitable for vignette output and test snapshots.
#'
#' @param object A `doseprop_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary doseprop_stats

summary.doseprop_stats <- function(object, ...) {
  metric_var_str <- object$config$metric_var
  cat("<doseprop_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(object$stats), ncol(object$stats)))
  cat(sprintf("  obs:   %d rows\n",
              if (is.data.frame(object$obs)) nrow(object$obs) else 0L))
  cat(sprintf("  config: metric_var = %s, exp_var = %s, dose_var = %s, ci = %s, method = %s\n",
              format(metric_var_str),
              format(object$config$exp_var),
              format(object$config$dose_var),
              format(object$config$ci),
              format(object$config$method)))
  cat("\n  per-metric:\n")
  for (i in seq_len(nrow(object$stats))) {
    cat(sprintf("    %s: %s -- %s\n",
                object$stats[[metric_var_str]][i],
                object$stats$PowerCI[i],
                object$stats$Interpretation[i]))
  }
  invisible(object)
}
