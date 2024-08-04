
check_df <- function(df){
  input_name <-deparse(substitute(df))
  output_warning <- paste0("argument `", input_name, "` must be a `data.frame`")
  if(!is.data.frame(df)){
    rlang::abort(message = output_warning)
  }
}

check_factor <- function(data, var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `factor`")
  var_vect <- data[,var]
  if(!is.factor(var_vect)){
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

check_capture <- function(mod, var){
  input_name1 <-deparse(substitute(mod))
  input_name2 <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name2, "` must be captured as output in `",input_name1,"`")
  captures <- mod$capture
  if(!var %in% captures){
    rlang::abort(message = output_warning)
  }
}
