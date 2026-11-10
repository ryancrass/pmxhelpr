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
#' replaced), `deprecated`, `vpc_facet`. Message kinds: `loq_inherit`,
#' `ignored_arg`.
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
