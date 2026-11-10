check_df <- function(df, name){
  if(!is.data.frame(df)){
    rlang::abort(message = paste0("argument `", name, "` must be a `data.frame`"))
  }
  if(nrow(df) == 0L){
    rlang::abort(message = paste0("argument `", name, "` must have at least one row"))
  }
}


check_numeric_strict <- function(var, name){
  if(any(is.na(var)) || !is.numeric(var)){
    rlang::abort(message = paste0("argument `", name, "` must be class `numeric`"))
  }
}


check_integer <- function(var, name){
  num <- suppressWarnings(as.integer(var))
  if(any(is.na(num)) || !is.numeric(num)){
    rlang::abort(message = paste0("argument `", name, "` must be coercible to class `integer`"))
  }
}


check_positive_integer <- function(var, name){
  if(!is.numeric(var) || length(var) != 1L || is.na(var) || var < 1 || var != round(var)){
    rlang::abort(message = paste0("argument `", name, "` must be a single whole number >= 1"))
  }
}


check_boolean <- function(var, name){
  if(!isTRUE(var) && !isFALSE(var)){
    rlang::abort(message = paste0("argument `", name, "` must be `TRUE` or `FALSE`"))
  }
}


check_varsindf <- function(data, vars, data_name, name){
  missing <- setdiff(vars, colnames(data))
  if(length(missing) >= 1){
    rlang::abort(message = paste0("argument `", name, "` must be variable(s) in `", data_name,
                                  "` (not found: ",
                                  paste0("'", missing, "'", collapse = ", "), ").",
                                  "\nAvailable columns: ", paste(colnames(data), collapse = ", ")))
  }
}


check_levelsinvar <- function(data, var, levels, name, levels_name){
  missing <- setdiff(levels, data[[var]])
  if(length(missing) >= 1){
    available <- unique(data[[var]])
    rlang::abort(message = paste0("argument `", levels_name, "` must be levels in variable `", name,
                                  "` (not found: ",
                                  paste0("'", missing, "'", collapse = ", "), ").",
                                  "\nAvailable levels: ", paste(available, collapse = ", ")))
  }
}


check_quantile_pair <- function(x, name) {
  if (!is.numeric(x) || length(x) != 2L || any(is.na(x))) {
    rlang::abort(paste0("argument `", name, "` must be a length-2 numeric vector with no NAs"))
  }
  if (any(x < 0) || any(x > 1)) {
    rlang::abort(paste0("argument `", name, "` values must be in [0, 1]"))
  }
  if (x[1] >= x[2]) {
    rlang::abort(paste0("argument `", name, "` must be ordered: ", name, "[1] < ", name, "[2]"))
  }
}


check_quantile_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0 || x >= 1) {
    rlang::abort(paste0("argument `", name, "` must be a single numeric value in (0, 1)"))
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
#' @param call A `call` object, typically `match.call()` evaluated in the
#'    wrapper's frame.
#' @param plot_only_args Character vector of argument names that ARE honored
#'    on the precomputed-stats path (e.g. `"style"`, `"shown"`, `"pcvpc"`).
#'    The matched data argument should also be included.
#' @param fn_name Character scalar, the wrapper's name, used in the error
#'    message.
#' @param stats_fn Character scalar, the stats function the caller should
#'    run instead (e.g. `"df_forest"`), used in the error message.
#'
#' @return `invisible(NULL)` on success.
#' @keywords internal
check_pipeline_args_dropped <- function(call, plot_only_args, fn_name, stats_fn) {
  passed <- names(call)
  passed <- passed[nzchar(passed)]                 # drop "" (function name, positional)
  passed <- setdiff(passed, plot_only_args)
  if (length(passed) > 0) {
    rlang::abort(paste0(
      "`", fn_name, "()` cannot accept pipeline arguments when `data` is a ",
      "precomputed `pmx_stats` object: ",
      paste(passed, collapse = ", "),
      ". Either: (a) call `", stats_fn, "()` with these ",
      "arguments and pass its result to `", fn_name, "()`; or (b) pass raw ",
      "data and let `", fn_name, "()` build the stats."
    ))
  }
  invisible(NULL)
}


#' Warn when a post-`EVID == 0` filter still contains multiple unique CMT values
#'
#' @description
#' Defensive check intended to be called immediately after an internal
#' `dplyr::filter(data, EVID == 0)` step. If the filtered data carries more
#' than one unique value of `CMT`, the function emits a `rlang::warn()`
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
  cmts <- unique(data[["CMT"]])
  if (length(cmts) <= 1L)          return(invisible(NULL))
  rlang::warn(c(
    paste0("Multiple unique values of `CMT` detected in `", name,
           "` after filtering to `EVID == 0`: ",
           paste(sort(cmts), collapse = ", "), "."),
    "i" = "Functions assume a single observation type per call.",
    "i" = "Pre-filter to a single observation compartment (e.g., `dplyr::filter(data, CMT == <n>)`) before passing to this function."
  ))
  invisible(NULL)
}


check_string <- function(x, name, null_ok = FALSE) {
  if (null_ok && is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    rlang::abort(message = paste0(
      "argument `", name, "` must be a single non-empty character string",
      if (null_ok) ", or `NULL`"
    ))
  }
  invisible(NULL)
}


## Names of a vector or list: present, non-empty, unique, and (when
## `allowed` is given) drawn from `allowed`, the value of argument
## `allowed_name`.
check_names <- function(x, name, what = "named vector",
                        allowed = NULL, allowed_name = NULL) {
  nms <- names(x)
  if (length(x) == 0L || is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    rlang::abort(message = paste0(
      "argument `", name, "` must be a non-empty ", what, " with non-empty names"
    ))
  }
  if (anyDuplicated(nms)) {
    rlang::abort(message = paste0(
      "argument `", name, "` has duplicate names: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", ")
    ))
  }
  extra <- setdiff(nms, allowed %||% nms)
  if (length(extra) > 0) {
    rlang::abort(message = paste0(
      "argument `", name, "` names columns that are not in `", allowed_name, "`: ",
      paste(extra, collapse = ", ")
    ))
  }
  invisible(NULL)
}


check_named_vector <- function(x, name, allowed = NULL, allowed_name = NULL) {
  if (!is.vector(x) || is.list(x)) {
    rlang::abort(message = paste0("argument `", name, "` must be a named atomic vector"))
  }
  check_names(x, name, allowed = allowed, allowed_name = allowed_name)
}
