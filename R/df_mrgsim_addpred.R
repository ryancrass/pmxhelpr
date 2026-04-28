#' Add population predictions (`PRED`) to a data.frame
#'
#' @description  `df_mrgsim_addpred()` is a wrapper function for
#' [mrgsolve::mrgsim_df()] and [mrgsolve::zero_re()] that returns
#' a data.frame with the addition of a new variable (`PRED`).
#'
#' @param data Input dataset.
#' @param model `mrgsolve` model object.
#' @param output_var Name of output from `model` to be captured as `PRED`
#'    after removing random effects with [mrgsolve::zero_re()]. Accepts bare names or strings. Default is `IPRED`.
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()].
#'
#' @return A data.frame with the same number of rows as `data` and on additional
#'  numeric variable `PRED`.
#' @export df_mrgsim_addpred
#'
#' @examples
#' pkmodel <- model_mread_load(model = "pkmodel")
#' data <- df_mrgsim_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
#'
df_mrgsim_addpred <- function(data,
                       model,
                       output_var = IPRED,
                       ...){

  output_var_str <- resolve_var(rlang::enquo(output_var))

  check_df(data, "data")
  check_mrgmod(model, "model")
  check_capture(model, output_var_str, "model", "output_var")

  data$PRED <- mrgsolve::mrgsim_df(x = mrgsolve::zero_re(model, ...),
                                   data = data, carry_out = output_var_str)[,output_var_str]
  return(data)
}
