#' Add count unique values of an identifier within each level of a variable in a data.frame
#'
#' @description  `df_addn()` returns a data.frame with a factor variable including count of individuals.
#'
#' @param data Input dataset.
#' @param grp_var Column to add counts to. Accepts bare names or strings.
#' @param id_var Column defining distinct values to count. Accepts bare names or strings. Default is `ID`.
#' @param sep Additional string separator to add between variable and count. Default is NULL.
#'
#' @return A data.frame with the same number of rows as `data` and a factor variable.
#' @export df_addn
#'
#' @examples
#' data <- df_addn(data = data_sad, grp_var = DOSE, id_var = ID)
#' unique(data$DOSE)
#'
#' data <- df_addn(data = data_sad, grp_var = DOSE, id_var = ID, sep = "mg")
#' unique(data$DOSE)
#'
df_addn <- function(data,
                    grp_var,
                    id_var = ID,
                    sep = NULL){

  grp_var <- rlang::ensym(grp_var)
  id_var  <- rlang::ensym(id_var)
  grp_var_str <- rlang::as_name(grp_var)
  id_var_str  <- rlang::as_name(id_var)

  check_df(data)
  check_varsindf(data, grp_var_str)
  check_varsindf(data, id_var_str)

  data_counts <- data |>
    dplyr::group_by(!!grp_var) |>
    dplyr::summarize(N = dplyr::n_distinct(!!id_var)) |>
    dplyr::ungroup()

  if(!is.null(sep)){
    out <- dplyr::left_join(data, data_counts) |>
      dplyr::mutate(tmp = factor(paste(!!grp_var,sep, paste0("(n=", N, ")"))))
  } else {
    out <- dplyr::left_join(data, data_counts) |>
      dplyr::mutate(tmp = factor(paste(!!grp_var, paste0("(n=", N, ")"))))
  }


  out[[grp_var_str]] <- out[["tmp"]]
  out <- dplyr::select(out, -N, -tmp)

  return(out)
}
