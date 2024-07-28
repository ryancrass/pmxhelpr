#' df_add_pred
#'
#' @param data simulation input dataset
#' @param model mrgsolve model object
#'
#' @return data.frame with additional variable PRED
#' @export
#'
#' @examples #need example
df_add_pred <- function(data, model){

  data$PRED <- mrgsolve::mrgsim_df(mrgsolve::zero_re(model),
                                   data = data, carry_out = "IPRED")$IPRED
}
