#' Add count unique values of an identifier within each level of a variable in a data.frame
#'
#' @description  `df_addn()` returns a data.frame with a factor variable including count of individuals.
#'
#' @param data Input dataset.
#' @param grp_var Variable to add counts to.
#' @param id_var Variable defining distinct values to count. Default is `"ID"`.
#' @param sep Additional string separator to add between variable and count. Default is NULL.
#' @param ... Other arguments passed to `factor()`.
#'
#' @return A data.frame with the same number of rows as `data` and a factor variable.
#' @export df_addn
#'
#' @examples
#' data <- df_addn(data = data_sad, grp_var = "DOSE", id_var = "ID")
#' unique(data$DOSE)
#'
#' data <- df_addn(data = data_sad, grp_var = "DOSE", id_var = "ID", sep = "mg")
#' unique(data$DOSE)
#'
df_addn <- function(data,
                    grp_var,
                    id_var = "ID",
                    sep = NULL){

  check_df(data)
  check_varsindf(data, grp_var)
  check_varsindf(data, id_var)

  data_counts <- data |>
    dplyr::group_by(!!dplyr::sym(grp_var)) |>
    dplyr::summarize(N = dplyr::n_distinct(!!dplyr::sym(id_var))) |>
    dplyr::ungroup()

  if(!is.null(sep)){
    out <- dplyr::left_join(data, data_counts) |>
      dplyr::mutate(tmp = factor(paste(!!dplyr::sym(grp_var),sep, paste0("(n=", N, ")"))))
  } else {
    out <- dplyr::left_join(data, data_counts) |>
      dplyr::mutate(tmp = factor(paste(!!dplyr::sym(grp_var), paste0("(n=", N, ")"))))
  }


  out[[grp_var]] <- out[["tmp"]]
  out <- dplyr::select(out, -N, -tmp)

  return(out)
}
