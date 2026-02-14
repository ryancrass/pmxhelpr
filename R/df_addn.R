#' Add count unique values of an identifier within each level of a variable in a data.frame
#'
#' @description  `df_addn()` returns a data.frame with a factor variable including count of individuals.
#'
#' @param data Input dataset.
#' @param grp_var Variable to add counts to.
#' @param id_var Variable defining distinct values to count. Default is `"ID"`.
#' @param sep Additional string separator to add between variable and count. Default is an empty string.
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
                    sep = ""){

  check_df(data)
  check_varsindf(data, grp_var)
  check_varsindf(data, id_var)

  n <- data |>
    dplyr::group_by(!!dplyr::sym(grp_var)) |>
    dplyr::summarize(n = dplyr::n_distinct(!!dplyr::sym(id_var))) |>
    dplyr::mutate(tmp = paste(!!dplyr::sym(grp_var), sep ,paste0("(n=", n, ")"))) |>
    dplyr::ungroup()

  data <- dplyr::left_join(data, n)
  data[[grp_var]] <- factor(data[[grp_var]], levels = n[[grp_var]], labels = n[["tmp"]])

  return(data)
}
