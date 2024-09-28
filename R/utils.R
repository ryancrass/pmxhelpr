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

check_mrgmod_outputvars <- function(mod, output_vars = c("IPRED", "DV")){
  mod_output_vars <-mrgsolve::outvars(mod)$capture
  output_warning <- paste0("mrg model must contain output variables `IPRED`, and `DV`.")
  if(!(output_vars[1] %in% mod_output_vars & output_vars[1] %in% mod_output_vars)){
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
  var_vect <- as.factor(data[,var])
  if(!is.factor(var_vect)){
    rlang::abort(message = output_warning)
  }
}

check_numeric <- function(var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `numeric`")
  num <- suppressWarnings(as.numeric(var))
  if(is.na(num) | !is.numeric(num)){
    rlang::abort(message = output_warning)
  }
}

check_integer <- function(var){
  input_name <-deparse(substitute(var))
  output_warning <- paste0("argument `", input_name, "` must be coercible to class `integer`")
  int <- suppressWarnings(as.integer(var))
  if(is.na(int) | !is.integer(int)){
    rlang::abort(message = output_warning)
  }
}

check_varsindf <- function(data, vars){
  input_name1 <-deparse(substitute(data))
  input_name2 <-deparse(substitute(vars))
  output_warning <- paste0("argument `", input_name2, "` must be variables in `",input_name1,"`")
  if(length(setdiff(vars, colnames(data)))>=1){
    rlang::abort(message = output_warning)
  }
}
