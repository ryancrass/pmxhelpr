list_update <- function(update=NULL, src, mess){

  update_list_name <-deparse(substitute(update))

  out <- src
  if(!is.null(update) & length(names(update))>0){
    for(i in seq(names(update))) {
      current_name <- names(update)[i]
      if(current_name %in% names(out)) {
        out[[current_name]] <- update[[current_name]]
      } else {
        warning(paste0("`", current_name,"` is not a valid element of ", update_list_name))
      }
    }
  }
  return(out)
}

check_modlib <- function(mod, mod_path){
  input_name <-mod
  output_warning <- paste0("`", input_name, "` does not exist in the pmxhelpr model library")
  if(!file.exists(mod_path)){
    rlang::abort(message = output_warning)
  }
}


check_df <- function(df){
  input_name <-deparse(substitute(df))
  output_warning <- paste0("argument `", input_name, "` must be a `data.frame`")
  if(!is.data.frame(df)){
    rlang::abort(message = output_warning)
  }
}

check_mrgmod <- function(mod){
  input_name <-deparse(substitute(mod))
  output_warning <- paste0("argument `", input_name, "` must be class `mrgmod`")
  if(!mrgsolve::is.mrgmod(mod)){
    rlang::abort(message = output_warning)
  }
}

check_mrgmod_outputvars <- function(mod, output_vars){
  mod_output_vars <-mrgsolve::outvars(mod)$capture
  output_warning <- paste0("mrg model must contain output variables specified in `output_vars`.")
  if(!(output_vars[["DV"]] %in% mod_output_vars) & !(output_vars[["IPRED"]] %in% mod_output_vars)){
    rlang::abort(message = output_warning)
  }
}

check_capture <- function(mod, var){
  input_name1 <-deparse(substitute(mod))
  input_name2 <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name2, "` must be captured as output in `",input_name1,"`")
  captures <- mod$capture
  if(!var %in% captures){
    rlang::abort(message = output_warning)
  }
}

check_factor <- function(data, var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `factor`")
  var_vect <- suppressWarnings(as.factor(data$var))
  if(!is.factor(var_vect)){
    rlang::abort(message = output_warning)
  }
}

check_numeric <- function(var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `numeric`")
  num <- suppressWarnings(as.numeric(var))
  if(sum(is.na(num)) | sum(!is.numeric(num))){
    rlang::abort(message = output_warning)
  }
}

check_numeric_strict <- function(var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be class `numeric`")
  if(sum(is.na(var)) | sum(!is.numeric(var))){
    rlang::abort(message = output_warning)
  }
}

check_integer <- function(var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `integer`")
  num <- suppressWarnings(as.integer(var))
  if(sum(is.na(num)) | sum(!is.numeric(num))){
    rlang::abort(message = output_warning)
  }
}

check_varsindf <- function(data, vars){
  input_name1 <-deparse(substitute(data))
  input_name2 <-deparse(substitute(vars))
  output_warning <- paste0("argument `", input_name2, "` must be variables in `",input_name1,"`")
  if(length(setdiff(vars, colnames(data)))>=1){
    if(!is.na(vars)){
      rlang::abort(message = output_warning)
    }
  }
}

check_levelsinvar <- function(data, var, levels){
  input_name1 <-deparse(substitute(var))
  input_name2 <-deparse(substitute(levels))
  vect <- data[[var]]
  output_warning <- paste0("argument `", input_name2, "` must be levels in variable `",input_name1,"`")
  if(sum(!levels %in% vect)){
    rlang::abort(message = output_warning)
  }
}

check_loq_method <- function(loq, loq_method, data) {

  if(is.null(loq_method)) {
    rlang::abort(message = "argument `loq_method` must be 0, 1, or 2")
  }

  if(!loq_method %in% c(0,1,2)) {
    rlang::abort(message = "argument `loq_method` must be 0, 1, or 2")
  }

  if(loq_method != 0) {
    if(is.null(loq)) {
      if(!"LLOQ" %in% colnames(data)) {
        rlang::abort(message = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
      }
    } else {
      num <- suppressWarnings(as.numeric(loq))
      if(is.na(num) | !is.numeric(num)) {
        rlang::abort(message = "If `loq_method` is 1 or 2, then a numeric variable `LLOQ` must be present in `data` \n or argument `loq` must specified and numeric or coercible to numeric")
      }
    }
  }
}


check_timeu <- function(var){
  values <- c("hours", "hrs", "hour", "hr", "h",
              "days", "dys", "day", "dy", "d",
              "weeks", "wks", "week", "wk", "w",
              "months", "mons", "mos", "month", "mo", "m")

  output_warning <- paste0("argument timeu must be one of: ", paste(values, collapse = ", "))
  if(! var %in% c(values)){
    rlang::abort(message = output_warning)
  }
}

check_lm <- function(fit){
  input_name <-deparse(substitute(fit))
  output_warning <- paste0("argument `", input_name, "` must be class `lm`")
  if(!"lm" %in% class(fit)){
    rlang::abort(message = output_warning)
  }
}
