#' model_load
#'
#' @param model model file name
#' @param ... additional arguments passed to mrgsolve::mread_cache
#'
#' @return mrgsolve model object
#' @export model_load
#'
#' @examples
#' model <- model_load("model")

model_load <- function(model, ...){
  model_path <- system.file("models", paste0(model, ".cpp") , package = "pmxhelpr")
  model_object <- mrgsolve::mread_cache(model_path, ...)
  return(model_object)
}
