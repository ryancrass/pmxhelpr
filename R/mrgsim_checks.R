check_modlib <- function(mod, mod_path){
  if(!file.exists(mod_path)){
    rlang::abort(message = paste0("argument `", mod, "` does not exist in the pmxhelpr model library"))
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
