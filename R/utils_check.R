check_modlib <- function(mod, mod_path){
  if(!file.exists(mod_path)){
    rlang::abort(message = paste0("argument `", mod, "` does not exist in the pmxhelpr model library"))
  }
}

check_df <- function(df, name){
  if(!is.data.frame(df)){
    rlang::abort(message = paste0("argument `", name, "` must be a `data.frame`"))
  }
  if(nrow(df) == 0L){
    rlang::abort(message = paste0("argument `", name, "` must have at least one row"))
  }
}

check_mrgmod <- function(mod, name){
  if(!mrgsolve::is.mrgmod(mod)){
    rlang::abort(message = paste0("argument `", name, "` must be class `mrgmod`"))
  }
}

check_mrgmod_outputvars <- function(mod, sim_dv_var_str, ipred_var_str){
  mod_output_vars <- mrgsolve::outvars(mod)$capture
  if(!(sim_dv_var_str %in% mod_output_vars) & !(ipred_var_str %in% mod_output_vars)){
    rlang::abort(message = "argument `model` must contain output variables specified in `sim_dv_var` or `ipred_var`")
  }
}

check_capture <- function(mod, var, mod_name, name){
  if(!var %in% mod$capture){
    rlang::abort(message = paste0("argument `", name, "` must be captured as output in `", mod_name, "`"))
  }
}

check_factor <- function(data, var, name){
  var_vect <- suppressWarnings(as.factor(data[[var]]))
  if(!is.factor(var_vect)){
    rlang::abort(message = paste0("argument `", name, "` must be coercible to class `factor`"))
  }
}

check_numeric <- function(var, name){
  num <- suppressWarnings(as.numeric(var))
  if(any(is.na(num)) || !is.numeric(num)){
    rlang::abort(message = paste0("argument `", name, "` must be coercible to class `numeric`"))
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

check_loq_method <- function(loq, loq_method, data) {
  aliases <- c(none = 0, postdose = 1, all = 2)
  if (is.character(loq_method) && loq_method %in% names(aliases)) {
    loq_method <- aliases[[loq_method]]
  }
  if(is.null(loq_method) || !loq_method %in% c(0, 1, 2)) {
    rlang::abort(message = 'argument `loq_method` must be 0, 1, 2, "none", "postdose", or "all"')
  }

  if(loq_method != 0) {
    if(is.null(loq)) {
      if(!"LLOQ" %in% colnames(data)) {
        rlang::abort(message = "argument `loq` must be numeric or variable `LLOQ` must be present in `data` when `loq_method` is 1 or 2")
      }
    } else {
      num <- suppressWarnings(as.numeric(loq))
      if(is.na(num) || !is.numeric(num)) {
        rlang::abort(message = "argument `loq` must be numeric or variable `LLOQ` must be present in `data` when `loq_method` is 1 or 2")
      }
    }
  }
  loq_method
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

check_color <- function(x, name) {
  if (is.null(x)) return(invisible())
  ok <- tryCatch({
    grDevices::col2rgb(x)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    rlang::abort(paste0("argument `", name, "` must be a valid color name or hex string (got ",
                        paste0("'", x, "'", collapse = ", "), ")"))
  }
}

check_size <- function(x, name) {
  if (is.null(x)) return(invisible())
  if (!is.numeric(x) || any(is.na(x)) || any(x < 0)) {
    rlang::abort(paste0("argument `", name, "` must be a non-negative numeric value"))
  }
}

check_shape <- function(x, name) {
  if (is.null(x)) return(invisible())
  if (is.numeric(x)) {
    if (any(is.na(x)) || any(x != as.integer(x)) || any(x < 0) || any(x > 25)) {
      rlang::abort(paste0("argument `", name, "` must be an integer in 0:25 or a character"))
    }
    return(invisible())
  }
  if (!is.character(x)) {
    rlang::abort(paste0("argument `", name, "` must be an integer in 0:25 or a character"))
  }
}

check_loglog_args <- function(method, ci, sigdigits) {
  if (!method %in% c("normal", "tdist")) {
    rlang::abort(message = "argument `method` must be `normal` or `tdist`")
  }
  if (!is.numeric(ci) || ci <= 0 || ci >= 1) {
    rlang::abort(message = "argument `ci` must be a numeric value between 0 and 1")
  }
  check_integer(sigdigits, "sigdigits")
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
#'    on the precomputed-stats path (e.g. `"theme"`, `"shown"`, `"pcvpc"`).
#'    The matched data argument should also be included.
#' @param fn_name Character scalar, the wrapper's name, used in the error
#'    message.
#'
#' @return `invisible(NULL)` on success.
#' @keywords internal
check_pipeline_args_dropped <- function(call, plot_only_args, fn_name) {
  passed <- names(call)
  passed <- passed[nzchar(passed)]                 # drop "" (function name, positional)
  passed <- setdiff(passed, plot_only_args)
  if (length(passed) > 0) {
    rlang::abort(paste0(
      "`", fn_name, "()` cannot accept pipeline arguments when `data` is a ",
      "precomputed `pmx_stats` object: ",
      paste(passed, collapse = ", "),
      ". Either: (a) call the underlying df_*stats() function with these ",
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
