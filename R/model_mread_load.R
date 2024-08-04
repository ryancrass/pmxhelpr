#' Load an mrgsolve model file from the internal model library
#'
#' @description  `model_mread_load()` returns an mrgsolve model object from the internal
#' model library in `pmxhelpr`
#'
#' @param model Model file name. Must be a string.
#' @param ... Additional arguments passed to [mrgsolve::mread_cache()].
#'
#' @return An `mrgsolve` model object.
#' @export model_mread_load
#'
#' @examples
#' model <- model_mread_load("model")

model_mread_load <- function(model, ...){
  model_path <- system.file("models", paste0(model, ".cpp") , package = "pmxhelpr")
  check_modlib(model, model_path)
  model_object <- mrgsolve::mread_cache(model_path, ...)
  return(model_object)
}
