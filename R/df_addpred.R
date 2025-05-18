#' Add population predictions (`PRED`) to a data.frame
#'
#' @description  `df_addpred()` is a wrapper function for
#' [mrgsolve::mrgsim_df()] and [mrgsolve::zero_re()] that returns
#' a data.frame with the addition of a new variable (`PRED`).
#'
#' @param data Input dataset.
#' @param model `mrgsolve` model object.
#' @param output_var Name of output from `model` to be captured as `PRED`
#'    after removing random effects with [mrgsolve::zero_re()]. Default is `"IPRED"`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()].
#'
#' @return A data.frame with the same number of rows as `data` and on additional
#'  numeric variable `PRED`.
#' @export df_addpred
#'
#' @examples
#' model <- model_mread_load(model = "model")
#' data <- df_addpred(data = data_sad, model = model)
#'
df_addpred <- function(data,
                       model,
                       output_var="IPRED",
                       ...){

  check_df(data)
  check_mrgmod(model)
  check_capture(model, output_var)

  data$PRED <- mrgsolve::mrgsim_df(x = mrgsolve::zero_re(model, ...),
                                   data = data, carry_out = output_var)[,output_var]
  return(data)
}
