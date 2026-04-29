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
  if(length(setdiff(vars, colnames(data))) >= 1){
    rlang::abort(message = paste0("argument `", name, "` must be variable(s) in `", data_name, "`"))
  }
}

check_levelsinvar <- function(data, var, levels, name, levels_name){
  if(any(!levels %in% data[[var]])){
    rlang::abort(message = paste0("argument `", levels_name, "` must be levels in variable `", name, "`"))
  }
}

check_loq_method <- function(loq, loq_method, data) {
  if(is.null(loq_method) || !loq_method %in% c(0, 1, 2)) {
    rlang::abort(message = "argument `loq_method` must be 0, 1, or 2")
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

check_loglog_args <- function(method, ci, sigdigits) {
  if (!method %in% c("normal", "tdist")) {
    rlang::abort(message = "argument `method` must be `normal` or `tdist`")
  }
  if (!is.numeric(ci) || ci <= 0 || ci >= 1) {
    rlang::abort(message = "argument `ci` must be a numeric value between 0 and 1")
  }
  check_integer(sigdigits, "sigdigits")
}
