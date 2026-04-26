#' Count the non-missing observations in each exact bin
#'
#' @description
#' `df_nobsbin()` is a helper function to count the number of missing
#'    and non-missing observations in exact bins.
#'
#' @param data Input dataset. Must contrain variable "CMT".
#' @param bin_var Binning variable. Accepts bare names or strings. Default is `NTIME`.
#' @param strat_vars Stratifying variables. Must be a character vector.
#'
#' @return A data.frame containing one row per unique combination of
#'    `bin_var` and `strat_vars` and new variables `n_obs`, a count
#'    of non-missing observations, and `n_miss`, a count of missing observations.
#' @export df_nobsbin
#'
#' @examples
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' df_nobsbin(data_sad_pk)
#'
df_nobsbin <- function(data,
                       bin_var = NTIME,
                       strat_vars = NULL){

  bin_var_str <- rlang::as_name(rlang::ensym(bin_var))

  check_df(data)
  check_varsindf(data, bin_var_str)
  check_varsindf(data, strat_vars)

  bin_count <- data |>
    dplyr::filter(EVID == 0) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(bin_var_str, strat_vars, "CMT")))) |>
    dplyr::summarize(n_obs = sum(MDV==0),
                     n_miss = sum(MDV==1)) |>
    dplyr::ungroup()

  return(bin_count)
}
