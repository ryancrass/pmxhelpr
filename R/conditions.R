#' Internal: raise a classed pmxhelpr condition
#'
#' Thin wrappers over [cli::cli_abort()], [cli::cli_warn()] and
#' [cli::cli_inform()] that give every condition the base class
#' (`pmxhelpr_error`, `pmxhelpr_warning`, `pmxhelpr_message`) and a kind
#' subclass `pmxhelpr_<type>_<kind>`, so callers and tests can catch
#' conditions by class rather than by message text.
#'
#' Error kinds:
#' * `bad_arg`: an argument has the wrong type or value.
#' * `missing_var`: a column or level is not in the data.
#' * `bad_stats`: a `pmx_stats` container is malformed.
#' * `pipeline_args`: pipeline arguments passed alongside precomputed stats.
#' * `model`: an mrgsolve model is missing, of the wrong class, or lacks
#'   required outputs.
#' * `bad_style`: style overrides cannot be applied.
#' * `bad_data`: the data contents are inconsistent in a way no single
#'   argument explains.
#'
#' Warning kinds: `data` (suspicious data contents), `ignored_arg` (an
#' argument has no or partial effect), `overwrite` (existing columns are
#' replaced), `vpc_facet`. Message kinds: `loq_inherit`, `ignored_arg`.
#' Deprecated arguments are signalled by lifecycle instead (class
#' `lifecycle_warning_deprecated`); see `use_deprecated_arg()`.
#'
#' The message is a cli format string, interpolated in the frame that raised
#' the condition (`.envir`). Never `paste0()` user values into it; interpolate
#' them with `{}`.
#'
#' @param message A cli format string, or a character vector of them with
#'    bullet names (`"x"`, `"i"`, `"!"`, ...).
#' @param kind Character scalar, the kind subclass.
#' @param ... Passed to the cli function.
#' @param call The call reported for an error; internal helpers pass their
#'    caller's `call` so errors name the exported function the user called.
#' @param .envir Environment in which to interpolate `message`.
#' @return `pmx_abort()` does not return; the others return `NULL`
#'    invisibly.
#' @noRd
pmx_abort <- function(message, kind, ..., call = rlang::caller_env(),
                      .envir = parent.frame()) {
  cli::cli_abort(message, ...,
                 class = c(paste0("pmxhelpr_error_", kind), "pmxhelpr_error"),
                 call = call, .envir = .envir)
}


#' @rdname pmx_abort
#' @noRd
pmx_warn <- function(message, kind, ..., .envir = parent.frame()) {
  cli::cli_warn(message, ...,
                class = c(paste0("pmxhelpr_warning_", kind), "pmxhelpr_warning"),
                .envir = .envir)
}


#' @rdname pmx_abort
#' @noRd
pmx_inform <- function(message, kind, ..., .envir = parent.frame()) {
  cli::cli_inform(message, ...,
                  class = c(paste0("pmxhelpr_message_", kind), "pmxhelpr_message"),
                  .envir = .envir)
}


## TRUE when the caller should use the deprecated `old` argument: warns via
## lifecycle (once per 8 hours by default), and aborts if both names were
## given. The caller tests whether each was supplied, so a bare-column (NSE)
## argument is never evaluated here:
##   if (use_deprecated_arg(!missing(irep_name), !missing(rep_var),
##                          "df_vpcstats", "irep_name", "rep_var")) {
##     rep_var_q <- rlang::enquo(irep_name)
##   }
use_deprecated_arg <- function(old_given, new_given, fn, old, new, when = "0.8.0",
                               call = rlang::caller_env(),
                               user_env = rlang::caller_env(2)) {
  if (!old_given) return(FALSE)
  if (new_given) {
    pmx_abort("Pass {.arg {new}} only; {.arg {old}} is its deprecated name.",
              "bad_arg", call = call)
  }
  lifecycle::deprecate_warn(when, sprintf("%s(%s)", fn, old), sprintf("%s(%s)", fn, new),
                            env = call, user_env = user_env)
  TRUE
}
