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
#' `ci`, `sigdigits`, `cov_name_ref`, `cov_level_ref`), and the first rows of
#' the stats frame. Inspect the full frame via `x$stats`; inspect run config
#' via `x$config`.
#'
#' @param x A `forest_stats` object.
#' @param n_head Number of rows of `x$stats` to preview. Default `3`; `0`
#'    omits the preview.
#' @param ... Passed to the underlying `print.data.frame()` call for the
#'    preview.
#'
#' @return `invisible(x)`.
#' @export
#' @method print forest_stats

print.forest_stats <- function(x, n_head = 3, ...) {
  forest_stats_header(x)
  if (n_head > 0 && nrow(x$stats) > 0) {
    cat(sprintf("\n  head(stats, %d):\n", n_head))
    print(utils::head(x$stats, n_head), ...)
  }
  cat("\n  Use `x$stats` for the full data.frame.\n")
  invisible(x)
}


forest_stats_header <- function(x) {
  pmx_stats_header(x, c("metric_name_var", "metric_value_var", "cov_name_var",
                        "cov_level_var", "ci", "sigdigits", "cov_name_ref",
                        "cov_level_ref"))
}



#' Summary method for `forest_stats`
#'
#' @description
#' Compact summary of a [df_forest()] result: the same header shown by
#' [print.forest_stats()], then one line per metric with its draw count `n`
#' and each covariate's levels in plot order. A covariate whose level repeats
#' its name (the reference panel) shows the name alone; reference copies
#' drawn in a covariate panel are marked `*`.
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
  level_label <- ifelse(s$layout == "distributed", paste0(cov_level, "*"), cov_level)
  metric_line <- function(m) {
    in_m <- metric == m
    covs <- unique(cov_name[in_m])
    panels <- vapply(covs, function(cn) {
      k <- in_m & cov_name == cn
      if (all(cov_level[k] == cn)) return(cn)
      paste0(cn, ": ", paste(level_label[k], collapse = ", "))
    }, character(1))
    n_tag <- if (is.null(s$n)) "" else {
      n_rng <- unique(range(s$n[in_m]))
      paste0("  n = ", paste(n_rng, collapse = "-"))
    }
    sprintf("    [%s]%s  %s\n", m, n_tag, paste(panels, collapse = " | "))
  }
  cat("\n  per metric:\n")
  cat(vapply(unique(metric), metric_line, character(1)), sep = "")
  if (any(s$layout == "distributed")) {
    cat("  * reference copy in a covariate panel (ref_layout = \"distributed\")\n")
  }
  invisible(object)
}
