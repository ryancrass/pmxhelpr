#' Construct a `pmx_stats` container
#'
#' @description
#' Constructor for the cacheable stats containers returned by
#' [df_vpcstats()], [df_doseprop()], and [df_forest()]. Each container is a named list with
#' three slots — `stats` (the per-row summary frame), `obs` (the observation
#' overlay used by plot builders, may be `NULL`), and `config` (named list of
#' run configuration consumed by the plot builders). The class vector is
#' `c(<subclass>, "pmx_stats")`. The container is checked with
#' [validate_pmx_stats()] at construction time, including the subclass
#' contract for `vpc_stats`, `doseprop_stats` and `forest_stats`.
#'
#' @param stats A `data.frame` of per-row summary statistics.
#' @param obs A `data.frame` of observation rows for the scatter overlay, or
#'    `NULL` when no overlay is used.
#' @param config A named `list` of run configuration (e.g. column names, CI
#'    width, replicate count).
#' @param subclass Character scalar naming the concrete subclass (e.g.
#'    `"vpc_stats"`, `"doseprop_stats"`, `"forest_stats"`).
#'
#' @family pmx stats class
#' @return An object of class `c(subclass, "pmx_stats")`.
#' @export pmx_stats

pmx_stats <- function(stats, obs = NULL, config = list(), subclass) {
  out <- structure(
    list(stats = stats, obs = obs, config = config),
    class = c(subclass, "pmx_stats")
  )
  validate_pmx_stats(out, arg = "stats")
  out
}



#' Test whether an object is a `pmx_stats` container
#'
#' @description
#' Predicate that returns `TRUE` if `x` carries the `"pmx_stats"` class. This
#' is the shared base class for [df_vpcstats()], [df_doseprop()], and
#' [df_forest()] outputs; use the subclass-specific predicates
#' [is_vpc_stats()], [is_doseprop_stats()], and [is_forest_stats()] when you
#' need to discriminate between pipelines.
#'
#' @param x Object to test.
#' @param strict Logical. When `TRUE`, additionally runs
#'    [validate_pmx_stats()], which applies the subclass contract, and
#'    returns `FALSE` on validation failure. Default `FALSE` (class-tag check
#'    only, cheap).
#'
#' @family pmx stats class
#' @return Logical scalar.
#' @export is_pmx_stats

is_pmx_stats <- function(x, strict = FALSE) {
  ok <- inherits(x, "pmx_stats")
  if (!ok || !isTRUE(strict)) return(ok)
  tryCatch({validate_pmx_stats(x); TRUE}, error = function(e) FALSE)
}



#' Validate a `pmx_stats` container
#'
#' @description
#' Checks that `x` is a well-formed `pmx_stats` container and returns it
#' invisibly, or aborts with an error of class `pmxhelpr_error_bad_stats`
#' naming what is wrong. Use it after editing a container (e.g. `x$stats`) or
#' building one by hand with [pmx_stats()]; [is_pmx_stats()] with
#' `strict = TRUE` runs the same checks but returns `TRUE`/`FALSE`.
#'
#' The generic dispatches on the container class:
#' * `vpc_stats`: the bin, count and quantile columns of `stats`; the
#'   `OBSDV`/`PC_OBSDV`/`TIME` columns of `obs`; the `n_replicates`, `ci`,
#'   `pi`, `loq` and `strat_var` config keys; and the `sim_prop_blq_*`
#'   columns when `config$loq` is set.
#' * `doseprop_stats`: the power-model columns of `stats`, an `obs` frame
#'   holding the `config` metric and dose columns, and the `metric_name_var`,
#'   `metric_value_var`, `dose_var` and `ci` config keys.
#' * `forest_stats`: the estimate, label, `is_ref` and `layout` columns of
#'   `stats` with their types and values, the `config` grouping columns, and
#'   valid `config$ci` and `config$sigdigits`.
#' * Any other object: `x` must carry the `pmx_stats` class and have a
#'   `stats` data.frame, an `obs` data.frame or `NULL`, and a `config` list.
#'   Packages defining their own `pmx_stats` subclass can add a method.
#'
#' @param x Object to validate.
#' @param ... Passed to methods; unused by the methods in pmxhelpr.
#' @param arg,call The argument name and call reported in errors.
#'
#' @family pmx stats class
#' @return `invisible(x)` on success.
#' @export
#'
#' @examples
#' fs <- df_forest(data_sad_pkforest)
#' validate_pmx_stats(fs)
#' fs$stats$est <- as.character(fs$stats$est)
#' try(validate_pmx_stats(fs))

validate_pmx_stats <- function(x, ..., arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  UseMethod("validate_pmx_stats")
}


#' @rdname validate_pmx_stats
#' @export
validate_pmx_stats.default <- function(x, ..., arg = rlang::caller_arg(x),
                                       call = rlang::caller_env()) {
  check_pmx_stats(x, arg = arg, call = call)
}


#' Internal helper: check the shared `pmx_stats` shape
#'
#' Asserts that `x` carries the `pmx_stats` class and exposes the `stats` /
#' `obs` / `config` slots in the expected shape. Subclass validators pass their
#' contract (class, required columns, required config keys) through the
#' optional arguments, so every container reports missing pieces with the same
#' messages, and then apply any checks of their own.
#'
#' @param x Object to validate.
#' @param subclass Optional subclass name `x` must carry (e.g.
#'    `"forest_stats"`).
#' @param source Name of the function that builds `subclass`, quoted in the
#'    class error (e.g. `"df_forest"`).
#' @param stats_cols Columns `x$stats` must contain.
#' @param obs_cols Columns `x$obs` must contain.
#' @param config_keys Keys `x$config` must contain.
#' @param require_obs Logical; whether `x$obs` must be a data.frame rather
#'    than `NULL`. Implied by a non-empty `obs_cols`.
#' @param arg,call The argument name and call reported in errors.
#'
#' @return `invisible(x)` on success.
#' @keywords internal
#' @noRd

check_pmx_stats <- function(x, subclass = NULL, source = NULL,
                            stats_cols = NULL, obs_cols = NULL,
                            config_keys = NULL,
                            require_obs = length(obs_cols) > 0,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (!is.null(subclass) && !inherits(x, subclass)) {
    pmx_abort("{.arg {arg}} must be a {.cls {subclass}} object (output of {.fn {source}}).",
              "bad_stats", call = call)
  }
  if (!inherits(x, "pmx_stats")) {
    pmx_abort("{.arg {arg}} must be a {.cls pmx_stats} object.", "bad_stats", call = call)
  }
  required <- c("stats", "obs", "config")
  missing  <- setdiff(required, names(x))
  if (length(missing) > 0) {
    pmx_abort("{.arg {arg}} is missing {cli::qty(missing)}slot{?s} {.var {missing}}.",
              "bad_stats", call = call)
  }
  if (!is.data.frame(x$stats)) {
    pmx_abort("{.code {arg}$stats} must be a {.cls data.frame}.", "bad_stats", call = call)
  }
  if (!is.null(x$obs) && !is.data.frame(x$obs)) {
    pmx_abort("{.code {arg}$obs} must be a {.cls data.frame} or {.code NULL}.", "bad_stats",
              call = call)
  }
  if (!is.list(x$config)) {
    pmx_abort("{.code {arg}$config} must be a {.cls list}.", "bad_stats", call = call)
  }
  if (isTRUE(require_obs) && !is.data.frame(x$obs)) {
    pmx_abort("{.arg {arg}} must contain {.var stats} and {.var obs} data frames.",
              "bad_stats", call = call)
  }
  missing_from <- function(have, need, what, noun) {
    miss <- setdiff(need, have)
    if (length(miss) > 0) {
      pmx_abort("{.code {arg}${what}} is missing required {noun}: {.var {miss}}.",
                "bad_stats", call = call)
    }
  }
  missing_from(colnames(x$stats), stats_cols, "stats", "columns")
  missing_from(colnames(x$obs), obs_cols, "obs", "columns")
  missing_from(names(x$config), config_keys, "config", "keys")
  invisible(x)
}


#' Internal helper: print the header shared by the `pmx_stats` subclasses
#'
#' Prints the class tag, the `stats` dimensions, the `obs` row count (when
#' `obs` is a data.frame), and the `config` values named in `config_keys` as
#' one `key = value` line. `NULL` values print as `NULL`, named or multi-value
#' vectors as an R `c()` call, and scalars as is.
#'
#' @param x A `pmx_stats` subclass object.
#' @param config_keys Names of the `x$config` entries to show, in order.
#'
#' @return `invisible(x)`.
#' @keywords internal
#' @noRd
pmx_stats_header <- function(x, config_keys) {
  fmt_value <- function(v) {
    if (is.null(v)) return("NULL")
    if (!is.null(names(v)) || length(v) > 1) return(paste(deparse(v), collapse = ""))
    format(v)
  }
  cat(sprintf("<%s>\n", class(x)[1]))
  cat(sprintf("  stats: %d rows x %d columns\n", nrow(x$stats), ncol(x$stats)))
  if (is.data.frame(x$obs)) cat(sprintf("  obs:   %d rows\n", nrow(x$obs)))
  vals <- vapply(config_keys, function(k) fmt_value(x$config[[k]]), character(1))
  cat(sprintf("  config: %s\n", paste(config_keys, "=", vals, collapse = ", ")))
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
