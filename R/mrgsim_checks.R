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


check_mrgmod_outputvars <- function(mod, dv_output_str, ipred_output_str,
                                    call = rlang::caller_env()){
  mod_output_vars <- mrgsolve::outvars(mod)$capture
  if(!(dv_output_str %in% mod_output_vars) & !(ipred_output_str %in% mod_output_vars)){
    pmx_abort("{.arg model} must capture the output variables named by {.arg dv_output} or {.arg ipred_output}.",
              "model", call = call)
  }
}


check_capture <- function(mod, var, mod_name, name, call = rlang::caller_env()){
  if(!var %in% mod$capture){
    pmx_abort("{.arg {name}} ({.val {var}}) must be captured as output in {.arg {mod_name}}.",
              "model", call = call)
  }
}
