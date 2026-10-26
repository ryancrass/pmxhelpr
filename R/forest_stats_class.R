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
#'   metric_name_var = "METRIC",
#'   cov_name_var = "COV",
#'   cov_level_var = "COVLVL",
#'   metric_value_var = "VALUE",
#'   cov_name_ref = NULL
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
#' `ci`, `sigdigits`, `cov_name_ref`, `cov_level_ref`), and the per-row stats
#' body. Inspect the underlying
#' frame directly via `x$stats`; inspect run config via `x$config`.
#'
#' @param x A `forest_stats` object.
#' @param ... Passed to the underlying `print.data.frame()` call for the body.
#'
#' @return `invisible(x)`.
#' @export
#' @method print forest_stats

print.forest_stats <- function(x, ...) {
  forest_stats_header(x)
  cat("\n  stats body:\n")
  print(x$stats, ...)
  invisible(x)
}


forest_stats_header <- function(x) {
  pmx_stats_header(x, c("metric_name_var", "cov_name_var", "cov_level_var",
                        "ci", "sigdigits", "cov_name_ref", "cov_level_ref"))
}



#' Summary method for `forest_stats`
#'
#' @description
#' Compact summary of a [df_forest()] result: the same header shown by
#' [print.forest_stats()], but the body is condensed to one line per row:
#' `[metric] covariate: level`, or the covariate alone where the level
#' repeats it (the standalone reference row), followed by the row's draw
#' count `n` when the container has one. Rows drawn in only one
#' reference layout are tagged `(consolidated)` or `(distributed)`; untagged
#' rows draw in both. Suitable for vignette output and test snapshots.
#'
#' @param object A `forest_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary forest_stats

summary.forest_stats <- function(object, ...) {
  forest_stats_header(object)
  s <- object$stats
  metric <- as.character(s[[object$config$metric_name_var]])
  cov_name <- as.character(s[[object$config$cov_name_var]])
  cov_level <- as.character(s[[object$config$cov_level_var]])
  row_label <- ifelse(cov_level == cov_name, cov_name,
                      paste0(cov_name, ": ", cov_level))
  n_tag <- if (is.null(s$n)) "" else paste0("  n = ", s$n)
  layout_tag <- ifelse(s$layout == "both", "", paste0("  (", s$layout, ")"))
  cat("\n  per-row:\n")
  cat(sprintf("    [%s] %s%s%s\n", metric, row_label, n_tag, layout_tag), sep = "")
  invisible(object)
}
