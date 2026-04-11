#' Perform prediction-correction of the dependent variable
#'
#' @description
#' `df_pcdv` is a helper function to perform prediction-correction
#'    of observed or simulated depedent variables.
#'
#' @param data Input dataset
#' @param bin_var Exact binning variable. Accepts bare names or strings. Default is `NTIME`.
#' @param strat_vars Stratifying variables. Default is `NULL`.
#' @param dvpred_vars Names of variables for the dependent variable and population model prediction. Must be named character vector.
#'    Defaults are `"PRED"` and `"DV"`.
#' @param lower_bound Lower bound of the dependent variable for prediction correction. Default is `0`.
#'
#' @return A data.frame containing one row per unique combination of
#'    `bin_var` and `strat_vars` and new variable `PCDV` containing
#'    prediction-corrected observations.
#' @export df_pcdv
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' data <- df_addpred(data_sad_pk, model)
#' simout <- df_pcdv(data, dvpred_vars = c(DV = "ODV", PRED = "PRED"))
#'
df_pcdv <- function(data,
                    bin_var = NTIME,
                    strat_vars = NULL,
                    dvpred_vars = c(PRED = "PRED",
                                    DV = "DV"),
                    lower_bound = 0) {

  bin_var_str <- rlang::as_name(rlang::ensym(bin_var))

  dvpred_vars <- list_update(dvpred_vars, c(PRED = "PRED",
                                            DV = "DV"))

  check_df(data)
  check_varsindf(data, bin_var_str)
  check_varsindf(data, strat_vars)
  check_varsindf(data, dvpred_vars[["PRED"]])
  check_varsindf(data, dvpred_vars[["DV"]])

  data <- data |>
    dplyr::rename(dplyr::all_of(dvpred_vars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var_str, strat_vars, "CMT")))) |>
    dplyr::mutate(PREDBIN = stats::median(PRED),
                  PCDV = lower_bound + (DV-lower_bound)*((PREDBIN-lower_bound)/(PRED-lower_bound))) |>
    dplyr::ungroup()

  return(data)
}
