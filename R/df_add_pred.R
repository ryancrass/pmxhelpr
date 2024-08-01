#' Add population predictions (`PRED`) to a data.frame
#'
#' @description  `df_add_pred()` is a wrapper function for
#' [mrgsolve::mrgsim_df()] and [mrgsolve::zero_re()] that returns
#' a data.frame with the addition of a new variable (`PRED`).
#'
#' @param data Input dataset.
#' @param model `mrgsolve` model object.
#' @param output_var Name of output from `model` to be captured as `PRED`
#'    after removing random effects with [mrgsolve::zero_re()].Default is `"IPRED"`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()].
#'
#' @return A data.frame with the same number of rows as `data` and on additional
#'  numeric variable `PRED`.
#' @export df_add_pred
#'
#' @examples
#' model <- model_load(model = "model")
#' data <- df_add_pred(data = data_sad, model = model)
#'
df_add_pred <- function(data, model, output_var="IPRED", ...){

  data$PRED <- mrgsolve::mrgsim_df(x = mrgsolve::zero_re(model, ...),
                                   data = data, carry_out = output_var)[,output_var]
  return(data)
}
