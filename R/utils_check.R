check_modlib <- function(mod, mod_path){
  if(!file.exists(mod_path)){
    rlang::abort(message = paste0("argument `", mod, "` does not exist in the pmxhelpr model library"))
  }
}

check_df <- function(df, name){
  if(!is.data.frame(df)){
    rlang::abort(message = paste0("argument `", name, "` must be a `data.frame`"))
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

check_timeu <- function(var){
  values <- c("hours", "hrs", "hour", "hr", "h",
              "days", "dys", "day", "dy", "d",
              "weeks", "wks", "week", "wk", "w",
              "months", "mons", "mos", "month", "mo", "m")

  if(!var %in% values){
    rlang::abort(message = paste0("argument `timeu` must be one of: ", paste(values, collapse = ", ")))
  }
}

check_lm <- function(fit, name){
  if(!"lm" %in% class(fit)){
    rlang::abort(message = paste0("argument `", name, "` must be class `lm`"))
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
