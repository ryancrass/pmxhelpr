#' Construct a `pmx_stats` container
#'
#' @description
#' Internal constructor for the cacheable stats containers returned by
#' [df_vpcstats()] and [df_doseprop()]. Each container is a named list with
#' three slots — `stats` (the per-row summary frame), `obs` (the observation
#' overlay used by plot builders, may be `NULL`), and `config` (named list of
#' run configuration consumed by the plot builders). The class vector is
#' `c(<subclass>, "pmx_stats")`. The structural shape is enforced by
#' [validate_pmx_stats()] at construction time.
#'
#' @param stats A `data.frame` of per-row summary statistics.
#' @param obs A `data.frame` of observation rows for the scatter overlay, or
#'    `NULL` when no overlay is used.
#' @param config A named `list` of run configuration (e.g. column names, CI
#'    width, replicate count).
#' @param subclass Character scalar naming the concrete subclass (e.g.
#'    `"vpc_stats"`, `"doseprop_stats"`).
#'
#' @return An object of class `c(subclass, "pmx_stats")`.
#' @keywords internal

pmx_stats <- function(stats, obs = NULL, config = list(), subclass) {
  out <- structure(
    list(stats = stats, obs = obs, config = config),
    class = c(subclass, "pmx_stats")
  )
  validate_pmx_stats(out)
  out
}



#' Test whether an object is a `pmx_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"pmx_stats"` class. This
#' is the shared base class for [df_vpcstats()] and [df_doseprop()] outputs;
#' use the subclass-specific predicates [is_vpc_stats()] and
#' [is_doseprop_stats()] when you need to discriminate between pipelines.
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    [validate_pmx_stats()] and returns `FALSE` on validation failure.
#'    Default `FALSE` (class-tag check only, cheap).
#'
#' @return Logical scalar.
#' @export is_pmx_stats

is_pmx_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "pmx_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_pmx_stats(x); TRUE}, error = function(e) FALSE)
}



#' Validate a `pmx_stats` container's structural shape
#'
#' @description
#' Internal helper. Asserts that `x` carries the `pmx_stats` class and exposes
#' the `stats` / `obs` / `config` slots in the expected shape. Subclass
#' validators delegate to this function for the structural checks before
#' applying their own column / config-key checks.
#'
#' @param x Object to validate.
#'
#' @return `invisible(x)` on success.
#' @keywords internal

validate_pmx_stats <- function(x) {
  if (!inherits(x, "pmx_stats")) {
    rlang::abort("`x` must be a `pmx_stats` object.")
  }
  required <- c("stats", "obs", "config")
  missing  <- setdiff(required, names(x))
  if (length(missing) > 0) {
    rlang::abort(paste0("`pmx_stats` object is missing slots: ",
                        paste(missing, collapse = ", ")))
  }
  if (!is.data.frame(x$stats)) {
    rlang::abort("`pmx_stats$stats` must be a `data.frame`.")
  }
  if (!is.null(x$obs) && !is.data.frame(x$obs)) {
    rlang::abort("`pmx_stats$obs` must be a `data.frame` or `NULL`.")
  }
  if (!is.list(x$config)) {
    rlang::abort("`pmx_stats$config` must be a `list`.")
  }
  invisible(x)
}



#' Print method for `pmx_stats`
#'
#' @description
#' Generic fallback print for `pmx_stats` containers. Subclasses
#' (`vpc_stats`, `doseprop_stats`) provide their own richer print methods;
#' this one is reached only by direct construction of the base class.
#'
#' @param x A `pmx_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(x)`.
#' @export
#' @method print pmx_stats

print.pmx_stats <- function(x, ...) {
  cat(sprintf("<pmx_stats: %s>\n", paste(class(x), collapse = ", ")))
  cat(sprintf("  stats: %d rows x %d columns\n",
              nrow(x$stats), ncol(x$stats)))
  cat(sprintf("  obs:   %d rows\n",
              if (is.data.frame(x$obs)) nrow(x$obs) else 0L))
  if (length(x$config) > 0) {
    cat(sprintf("  config: %s\n", paste(names(x$config), collapse = ", ")))
  }
  invisible(x)
}



#' Summary method for `pmx_stats`
#'
#' @description
#' Generic fallback summary for `pmx_stats` containers; delegates to
#' [print.pmx_stats()]. Subclasses provide their own summary methods.
#'
#' @param object A `pmx_stats` object.
#' @param ... Currently unused.
#'
#' @return `invisible(object)`.
#' @export
#' @method summary pmx_stats

summary.pmx_stats <- function(object, ...) {
  print.pmx_stats(object, ...)
}



#' Coerce a `pmx_stats` object to a data.frame
#'
#' @description
#' Returns the `stats` slot as a plain `data.frame`. Use `x$obs` separately
#' for the observation overlay and `x$config` for the run configuration.
#'
#' @param x A `pmx_stats` object.
#' @param row.names,optional Standard `as.data.frame` arguments (unused).
#' @param ... Currently unused.
#'
#' @return A `data.frame` (the `stats` slot).
#' @export
#' @method as.data.frame pmx_stats

as.data.frame.pmx_stats <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$stats)
}
