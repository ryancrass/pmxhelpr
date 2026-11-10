check_df <- function(df, name, call = rlang::caller_env()){
  if(!is.data.frame(df)){
    pmx_abort("{.arg {name}} must be a {.cls data.frame}.", "bad_arg", call = call)
  }
  if(nrow(df) == 0L){
    pmx_abort("{.arg {name}} must have at least one row.", "bad_arg", call = call)
  }
}


check_numeric_strict <- function(var, name, call = rlang::caller_env()){
  if(any(is.na(var)) || !is.numeric(var)){
    pmx_abort("{.arg {name}} must be {.cls numeric} with no missing values.", "bad_arg",
              call = call)
  }
}


check_integer <- function(var, name, call = rlang::caller_env()){
  num <- suppressWarnings(as.integer(var))
  if(any(is.na(num)) || !is.numeric(num)){
    pmx_abort("{.arg {name}} must be coercible to {.cls integer}.", "bad_arg", call = call)
  }
}


check_positive_integer <- function(var, name, call = rlang::caller_env()){
  if(!is.numeric(var) || length(var) != 1L || is.na(var) || var < 1 || var != round(var)){
    pmx_abort("{.arg {name}} must be a single whole number >= 1.", "bad_arg", call = call)
  }
}


check_boolean <- function(var, name, call = rlang::caller_env()){
  if(!isTRUE(var) && !isFALSE(var)){
    pmx_abort("{.arg {name}} must be {.code TRUE} or {.code FALSE}.", "bad_arg", call = call)
  }
}


check_varsindf <- function(data, vars, data_name, name, call = rlang::caller_env()){
  missing <- setdiff(vars, colnames(data))
  if(length(missing) >= 1){
    pmx_abort(c(
      "{.arg {name}} must name {cli::qty(missing)}variable{?s} in {.arg {data_name}}.",
      "x" = "Not found: {.val {missing}}.",
      "i" = "Available columns: {.field {colnames(data)}}."
    ), "missing_var", call = call)
  }
}


check_levelsinvar <- function(data, var, levels, name, levels_name,
                              call = rlang::caller_env()){
  missing <- setdiff(levels, data[[var]])
  if(length(missing) >= 1){
    available <- unique(data[[var]])
    pmx_abort(c(
      "{.arg {levels_name}} must be levels of variable {.var {name}}.",
      "x" = "Not found: {.val {missing}}.",
      "i" = "Available levels: {.val {available}}."
    ), "missing_var", call = call)
  }
}


check_quantile_pair <- function(x, name, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 2L || any(is.na(x))) {
    pmx_abort("{.arg {name}} must be a length-2 numeric vector with no missing values.",
              "bad_arg", call = call)
  }
  if (any(x < 0) || any(x > 1)) {
    pmx_abort("{.arg {name}} values must be in [0, 1].", "bad_arg", call = call)
  }
  if (x[1] >= x[2]) {
    pmx_abort("{.arg {name}} must be ordered: {name}[1] < {name}[2].", "bad_arg",
              call = call)
  }
}


check_quantile_scalar <- function(x, name, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0 || x >= 1) {
    pmx_abort("{.arg {name}} must be a single numeric value in (0, 1).", "bad_arg",
              call = call)
  }
}


#' Internal helper: abort if pipeline arguments are passed on the
#' precomputed-stats path.
#'
#' Plot wrappers like [plot_vpc_cont()] and [plot_doseprop()] accept either
#' raw data or a precomputed `pmx_stats` container. On the cached path the
#' pipeline never re-runs, so any pipeline argument the user supplied would
#' be silently ignored. This helper inspects the caller's `match.call()`,
#' subtracts the explicitly-allowed plot-only arguments, and aborts when the
#' remainder is non-empty — making the silent-shadow case audible.
#'
#' @param mc A `call` object, typically `match.call()` evaluated in the
#'    wrapper's frame.
#' @param plot_only_args Character vector of argument names that ARE honored
#'    on the precomputed-stats path (e.g. `"style"`, `"shown"`, `"pcvpc"`).
#'    The matched data argument should also be included.
#' @param fn_name Character scalar, the wrapper's name, used in the error
#'    message.
#' @param stats_fn Character scalar, the stats function the caller should
#'    run instead (e.g. `"df_forest"`), used in the error message.
#' @param call The call reported in the error.
#'
#' @return `invisible(NULL)` on success.
#' @keywords internal
check_pipeline_args_dropped <- function(mc, plot_only_args, fn_name, stats_fn,
                                        call = rlang::caller_env()) {
  passed <- names(mc)
  passed <- passed[nzchar(passed)]                 # drop "" (function name, positional)
  passed <- setdiff(passed, plot_only_args)
  if (length(passed) > 0) {
    pmx_abort(c(
      "{.fn {fn_name}} cannot accept pipeline arguments when {.arg data} is a precomputed {.cls pmx_stats} object.",
      "x" = "Pipeline argument{?s} passed: {.arg {passed}}.",
      "i" = "Either call {.fn {stats_fn}} with {cli::qty(passed)}{?this argument/these arguments} and pass its result to {.fn {fn_name}}, or pass raw data and let {.fn {fn_name}} build the stats."
    ), "pipeline_args", call = call)
  }
  invisible(NULL)
}


#' Warn when a post-`EVID == 0` filter still contains multiple unique CMT values
#'
#' @description
#' Defensive check intended to be called immediately after an internal
#' `dplyr::filter(data, EVID == 0)` step. If the filtered data carries more
#' than one unique value of `CMT`, the function emits a warning
#' recommending the caller pre-filter to a single observation compartment.
#' No-op when the `CMT` column is absent or the data is empty.
#'
#' @param data Filtered data frame.
#' @param name Character scalar, name of the upstream argument used in the
#'    warning text. Defaults to `"data"`.
#' @return `invisible(NULL)`.
#' @keywords internal
check_single_cmt <- function(data, name = "data") {
  if (!"CMT" %in% colnames(data)) return(invisible(NULL))
  if (nrow(data) == 0L)            return(invisible(NULL))
  cmts <- sort(unique(data[["CMT"]]))
  if (length(cmts) <= 1L)          return(invisible(NULL))
  pmx_warn(c(
    "Multiple unique values of {.var CMT} detected in {.arg {name}} after filtering to {.code EVID == 0}: {.val {cmts}}.",
    "i" = "Functions assume a single observation type per call.",
    "i" = "Pre-filter to a single observation compartment (e.g., {.code dplyr::filter(data, CMT == <n>)}) before passing to this function."
  ), "data")
  invisible(NULL)
}


check_string <- function(x, name, null_ok = FALSE, call = rlang::caller_env()) {
  if (null_ok && is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    or_null <- if (null_ok) ", or {.code NULL}" else ""
    pmx_abort(paste0("{.arg {name}} must be a single non-empty character string", or_null, "."),
              "bad_arg", call = call)
  }
  invisible(NULL)
}


## Names of a vector or list: present, non-empty, unique, and (when
## `allowed` is given) drawn from `allowed`, the value of argument
## `allowed_name`.
check_names <- function(x, name, what = "named vector",
                        allowed = NULL, allowed_name = NULL,
                        call = rlang::caller_env()) {
  nms <- names(x)
  if (length(x) == 0L || is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    pmx_abort("{.arg {name}} must be a non-empty {what} with non-empty names.", "bad_arg",
              call = call)
  }
  if (anyDuplicated(nms)) {
    dups <- unique(nms[duplicated(nms)])
    pmx_abort("{.arg {name}} has duplicate names: {.val {dups}}.", "bad_arg", call = call)
  }
  extra <- setdiff(nms, allowed %||% nms)
  if (length(extra) > 0) {
    pmx_abort("{.arg {name}} names columns that are not in {.arg {allowed_name}}: {.val {extra}}.",
              "bad_arg", call = call)
  }
  invisible(NULL)
}


check_named_vector <- function(x, name, allowed = NULL, allowed_name = NULL,
                               call = rlang::caller_env()) {
  if (!is.vector(x) || is.list(x)) {
    pmx_abort("{.arg {name}} must be a named atomic vector.", "bad_arg", call = call)
  }
  check_names(x, name, allowed = allowed, allowed_name = allowed_name, call = call)
}
