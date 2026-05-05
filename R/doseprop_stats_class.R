#' Test whether an object is a `doseprop_stats` data.frame
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"doseprop_stats"` class
#' -- i.e., it is the data.frame returned by [df_doseprop()] (also accepted by
#' [plot_doseprop()] and [plot_build_doseprop()] on the precomputed-stats
#' fast path).
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#' @export is_doseprop_stats
#'
#' @examples
#' stats <- df_doseprop(data_sad_nca, metrics = c("aucinf.obs", "cmax"))
#' is_doseprop_stats(stats)             # TRUE
#' is_doseprop_stats(as.data.frame(stats))  # FALSE -- class stripped

is_doseprop_stats <- function(x) {
  inherits(x, "doseprop_stats")
}



#' Print method for `doseprop_stats`
#'
#' @description
#' Focused summary of a [df_doseprop()] result: object dimensions, the
#' regression configuration attributes (`metric_var`, `exp_var`, `dose_var`,
#' `ci`, `method`), the number of observation rows attached for the plot
#' scatter overlay, and the per-metric stats body. Inspect attributes
#' directly via `attr(x, "obs")`, `attr(x, "metric_var")`, etc.
#'
#' @param x A `doseprop_stats` object.
#' @param ... Passed to the underlying `print.data.frame()` call for the body.
#'
#' @return `invisible(x)`.
#' @export
#' @method print doseprop_stats

print.doseprop_stats <- function(x, ...) {
  obs <- attr(x, "obs")
  cat("<doseprop_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n", nrow(x), ncol(x)))
  cat(sprintf("  obs:   %d rows\n",
              if (is.data.frame(obs)) nrow(obs) else 0L))
  cat(sprintf("  attributes: metric_var = %s, exp_var = %s, dose_var = %s, ci = %s, method = %s\n",
              format(attr(x, "metric_var")),
              format(attr(x, "exp_var")),
              format(attr(x, "dose_var")),
              format(attr(x, "ci")),
              format(attr(x, "method"))))
  cat("\n  stats body:\n")
  body <- x
  class(body) <- "data.frame"
  attr(body, "obs")        <- NULL
  attr(body, "metric_var") <- NULL
  attr(body, "exp_var")    <- NULL
  attr(body, "dose_var")   <- NULL
  attr(body, "ci")         <- NULL
  attr(body, "method")     <- NULL
  print(body, ...)
  cat("\n  Use `attr(x, \"obs\")` for the observation overlay.\n")
  invisible(x)
}



#' Summary method for `doseprop_stats`
#'
#' @description
#' Compact summary of a [df_doseprop()] result: the same header and
#' configuration attributes shown by [print.doseprop_stats()], but the body
#' is condensed to one line per metric using the `PowerCI` and
#' `Interpretation` columns. Suitable for vignette output and test
#' snapshots.
#'
#' @param object A `doseprop_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary doseprop_stats

summary.doseprop_stats <- function(object, ...) {
  obs <- attr(object, "obs")
  metric_var_str <- attr(object, "metric_var")
  cat("<doseprop_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n", nrow(object), ncol(object)))
  cat(sprintf("  obs:   %d rows\n",
              if (is.data.frame(obs)) nrow(obs) else 0L))
  cat(sprintf("  attributes: metric_var = %s, exp_var = %s, dose_var = %s, ci = %s, method = %s\n",
              format(metric_var_str),
              format(attr(object, "exp_var")),
              format(attr(object, "dose_var")),
              format(attr(object, "ci")),
              format(attr(object, "method"))))
  cat("\n  per-metric:\n")
  for (i in seq_len(nrow(object))) {
    cat(sprintf("    %s: %s -- %s\n",
                object[[metric_var_str]][i],
                object$PowerCI[i],
                object$Interpretation[i]))
  }
  invisible(object)
}



#' Coerce a `doseprop_stats` object to a data.frame
#'
#' @description
#' Drops the `"doseprop_stats"` class while preserving the regression
#' configuration attributes (`obs`, `metric_var`, `exp_var`, `dose_var`,
#' `ci`, `method`). Use when downstream tooling expects a plain `data.frame`
#' but you still want metadata recoverable via `attr()`.
#'
#' @param x A `doseprop_stats` object.
#' @param row.names,optional Standard `as.data.frame` arguments (unused).
#' @param ... Currently unused.
#'
#' @return A plain `data.frame` with the same columns and rows.
#' @export
#' @method as.data.frame doseprop_stats

as.data.frame.doseprop_stats <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}
