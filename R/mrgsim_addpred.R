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
#' @param ... Additional arguments passed to [mrgsolve::mrgsim_df()]. A
#'    `carry_out` is merged with `output_var`. Arguments that change the
#'    number of output rows (e.g. `obsonly`) are an error, since `PRED` is
#'    matched to `data` by row.
#'
#' @family mrgsolve wrappers
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
                       output_var = "IPRED",
                       ...){

  output_var_str <- resolve_var(rlang::enquo(output_var))

  check_df(data, "data")
  check_mrgmod(model, "model")
  check_capture(model, output_var_str, "model", "output_var")

  if ("PRED" %in% colnames(data)) {
    rlang::warn("`data` already contains a `PRED` column; it will be overwritten")
  }

  ## `...` goes to mrgsim_df(); a user `carry_out` is merged with the output
  ## captured as PRED rather than clashing with it.
  dots <- list(...)
  carry_out <- unique(c(output_var_str, dots$carry_out))
  dots$carry_out <- NULL
  sim <- do.call(mrgsolve::mrgsim_df, c(
    list(x = mrgsolve::zero_re(model), data = data, carry_out = carry_out),
    dots
  ))
  ## PRED is matched to `data` by row position.
  if (nrow(sim) != nrow(data)) {
    rlang::abort(paste0(
      "the simulation returned ", nrow(sim), " rows for ", nrow(data),
      " rows of `data`; `df_mrgsim_addpred()` needs one output row per input ",
      "row, so drop arguments in `...` that add or remove rows (e.g. `obsonly`)."
    ))
  }
  data$PRED <- sim[[output_var_str]]
  return(data)
}
