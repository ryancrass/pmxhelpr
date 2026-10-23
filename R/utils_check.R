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

#' Internal helper: resolve and validate the `style` argument
#'
#' Every plot builder accepts `style = NULL` (use the family preset) or a
#' [ggstylekit::style_spec()] object. This helper applies the default and
#' aborts early, with a pmxhelpr-branded message, when `style` is anything
#' else; without it, a bad `style` fails deep inside `ggstylekit` or, for
#' [plot_vpc_legend()], with an uninformative R error.
#'
#' @param style `NULL` or a `ggstylekit_style_spec` object.
#' @param default A function returning the family preset (e.g. [style_dvtime]),
#'    called only when `style` is `NULL`.
#'
#' @return A `ggstylekit_style_spec` object.
#' @keywords internal
resolve_style <- function(style, default) {
  if (is.null(style)) return(default())
  if (!inherits(style, "ggstylekit_style_spec")) {
    rlang::abort(paste0(
      "argument `style` must be a `ggstylekit::style_spec()` object, ",
      "typically from a `style_*()` preset (got <",
      paste(class(style), collapse = "/"), ">)"))
  }
  style
}

check_loglog_args <- function(method, ci, sigdigits) {
  if (!method %in% c("normal", "tdist")) {
    rlang::abort(message = "argument `method` must be `normal` or `tdist`")
  }
  check_quantile_scalar(ci, "ci")
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
#'    on the precomputed-stats path (e.g. `"style"`, `"shown"`, `"pcvpc"`).
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

check_ref <- function(ref) {
  if (is.null(ref) || identical(ref, "reference")) return(invisible(NULL))
  if (!is.numeric(ref) || length(ref) != 1L || is.na(ref)) {
    rlang::abort(message = "argument `ref` must be a single numeric value, `\"reference\"`, or `NULL`")
  }
  invisible(NULL)
}

check_ref_layout <- function(ref_layout) {
  if (is.null(ref_layout)) return(invisible(NULL))
  if (!is.character(ref_layout) || length(ref_layout) != 1L ||
      !ref_layout %in% c("distributed", "consolidated")) {
    rlang::abort(message = "argument `ref_layout` must be `\"distributed\"`, `\"consolidated\"`, or `NULL`")
  }
  invisible(NULL)
}

check_ref_band <- function(ref_band, ref_band_type = "absolute") {
  if (is.null(ref_band)) return(invisible(NULL))
  if (!is.numeric(ref_band) || length(ref_band) != 2L || any(is.na(ref_band))) {
    rlang::abort(message = "argument `ref_band` must be a length-2 numeric vector with no NAs")
  }
  if (ref_band[1] >= ref_band[2]) {
    rlang::abort(message = "argument `ref_band` must be ordered: ref_band[1] < ref_band[2]")
  }
  if (ref_band_type == "multiplicative" && any(ref_band <= 0)) {
    rlang::abort(message = "argument `ref_band` must be positive when `ref_band_type = \"multiplicative\"`")
  }
  invisible(NULL)
}

check_forest_metric <- function(metric, metric_values, metric_name_var_str) {
  if (is.null(metric)) {
    if (length(metric_values) != 1L) {
      rlang::abort(message = paste0(
        "argument `metric` is required when `stats$stats` contains multiple values of `",
        metric_name_var_str, "` (", paste(metric_values, collapse = ", "), "). ",
        "Pass `metric = <one of the listed values>` to pick which to render."
      ))
    }
    return(metric_values)
  }
  check_string(metric, "metric", null_ok = TRUE)
  if (!metric %in% metric_values) {
    rlang::abort(message = paste0(
      "argument `metric` (", metric, ") not found in `", metric_name_var_str,
      "`. Available: ", paste(metric_values, collapse = ", ")
    ))
  }
  metric
}


check_character_vars <- function(x, name) {
  if (!is.character(x) || length(x) == 0L) {
    rlang::abort(message = paste0(
      "argument `", name, "` must be a non-empty character vector of column names"
    ))
  }
  if (any(is.na(x)) || any(!nzchar(x))) {
    rlang::abort(message = paste0(
      "argument `", name, "` must not contain `NA` or empty strings"
    ))
  }
  if (anyDuplicated(x)) {
    rlang::abort(message = paste0(
      "argument `", name, "` has duplicate entries: ",
      paste(unique(x[duplicated(x)]), collapse = ", ")
    ))
  }
  invisible(NULL)
}

check_cov_val_ref <- function(cov_val_ref, cov_vars) {
  check_named_vector(cov_val_ref, "cov_val_ref",
                     allowed = cov_vars, allowed_name = "cov_vars")
  missing_cov <- setdiff(cov_vars, names(cov_val_ref))
  if (length(missing_cov) > 0) {
    rlang::abort(message = paste0(
      "argument `cov_val_ref` is missing a reference value for: ",
      paste(missing_cov, collapse = ", "),
      ". One entry is required per `cov_vars` column."
    ))
  }
  invisible(NULL)
}

check_cov_labels <- function(cov_labels, cov_vars) {
  if (is.null(cov_labels)) return(invisible(NULL))
  if (!is.list(cov_labels)) {
    rlang::abort(message = "argument `cov_labels` must be a named list (or `NULL`)")
  }
  check_names(cov_labels, "cov_labels", what = "named list",
              allowed = cov_vars, allowed_name = "cov_vars")
  for (cn in names(cov_labels)) {
    lab <- cov_labels[[cn]]
    if (is.function(lab)) next
    if (is.character(lab) && !is.null(names(lab)) &&
        !any(is.na(names(lab))) && all(nzchar(names(lab)))) {
      next
    }
    rlang::abort(message = paste0(
      "element `", cn, "` of argument `cov_labels` must be a function or a ",
      "named character vector keyed on the covariate values"
    ))
  }
  invisible(NULL)
}
