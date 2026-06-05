#' Test whether an object is a `forest_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"forest_stats"` class
#' -- i.e., it is the container returned by [df_forest()] (also accepted by
#' [plot_forest()] and [plot_build_forest()] on the precomputed-stats
#' fast path).
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    `validate_forest_stats()` and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @family forest plot
#' @return Logical scalar.
#' @export is_forest_stats
#'
#' @examples
#' set.seed(1)
#' draws <- expand.grid(
#'   METRIC = c("AUC", "CMAX"),
#'   COV = c("WTBL", "AGE"),
#'   COVLVL = c("Low", "High"),
#'   REP = 1:50,
#'   stringsAsFactors = FALSE
#' )
#' draws$VALUE <- rlnorm(nrow(draws), 0, 0.2)
#' stats <- df_forest(
#'   draws,
#'   metric_name_var    = "METRIC",
#'   cov_name_var  = "COV",
#'   cov_level_var = "COVLVL",
#'   metric_value_var     = "VALUE",
#'   replicate_var = "REP"
#' )
#' is_forest_stats(stats)                  # TRUE
#' is_forest_stats(as.data.frame(stats))   # FALSE -- coerced to plain frame

is_forest_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "forest_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_forest_stats(x); TRUE}, error = function(e) FALSE)
}



#' Print method for `forest_stats`
#'
#' @description
#' Focused summary of a [df_forest()] result: object dimensions, the
#' configuration values (`metric_name_var`, `cov_name_var`, `cov_level_var`,
#' `statistic`, `ci`), and the per-row stats body. Inspect the underlying
#' frame directly via `x$stats`; inspect run config via `x$config`.
#'
#' @param x A `forest_stats` object.
#' @param ... Passed to the underlying `print.data.frame()` call for the body.
#'
#' @return `invisible(x)`.
#' @export
#' @method print forest_stats

print.forest_stats <- function(x, ...) {
  cat("<forest_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(x$stats), ncol(x$stats)))
  cat(sprintf("  config: metric_name_var = %s, cov_name_var = %s, cov_level_var = %s, statistic = %s, ci = %s\n",
              format(x$config$metric_name_var),
              format(x$config$cov_name_var),
              format(x$config$cov_level_var),
              format(x$config$statistic),
              format(x$config$ci)))
  cat("\n  stats body:\n")
  print(x$stats, ...)
  invisible(x)
}



#' Summary method for `forest_stats`
#'
#' @description
#' Compact summary of a [df_forest()] result: the same header shown by
#' [print.forest_stats()], but the body is condensed to one line per row
#' using the `y_label` column. Suitable for vignette output and test
#' snapshots.
#'
#' @param object A `forest_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary forest_stats

summary.forest_stats <- function(object, ...) {
  cat("<forest_stats>\n")
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(object$stats), ncol(object$stats)))
  cat(sprintf("  config: metric_name_var = %s, cov_name_var = %s, cov_level_var = %s, statistic = %s, ci = %s\n",
              format(object$config$metric_name_var),
              format(object$config$cov_name_var),
              format(object$config$cov_level_var),
              format(object$config$statistic),
              format(object$config$ci)))
  metric_name_var_str <- object$config$metric_name_var
  cat("\n  per-row:\n")
  for (i in seq_len(nrow(object$stats))) {
    cat(sprintf("    [%s] %s\n",
                object$stats[[metric_name_var_str]][i],
                object$stats$y_label[i]))
  }
  invisible(object)
}
