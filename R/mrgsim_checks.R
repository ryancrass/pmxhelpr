check_modlib <- function(mod, mod_path, call = rlang::caller_env()){
  if(!file.exists(mod_path)){
    pmx_abort("Model {.val {mod}} does not exist in the pmxhelpr model library.", "model",
              call = call)
  }
}


check_mrgmod <- function(mod, name, call = rlang::caller_env()){
  if(!mrgsolve::is.mrgmod(mod)){
    pmx_abort("{.arg {name}} must be class {.cls mrgmod}.", "model", call = call)
  }
}


check_mrgmod_outputvars <- function(mod, sim_dv_var_str, ipred_var_str,
                                    call = rlang::caller_env()){
  mod_output_vars <- mrgsolve::outvars(mod)$capture
  if(!(sim_dv_var_str %in% mod_output_vars) & !(ipred_var_str %in% mod_output_vars)){
    pmx_abort("{.arg model} must capture the output variables named by {.arg sim_dv_var} or {.arg ipred_var}.",
              "model", call = call)
  }
}


check_capture <- function(mod, var, mod_name, name, call = rlang::caller_env()){
  if(!var %in% mod$capture){
    pmx_abort("{.arg {name}} ({.val {var}}) must be captured as output in {.arg {mod_name}}.",
              "model", call = call)
  }
}
