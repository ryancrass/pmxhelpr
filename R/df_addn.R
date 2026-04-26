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
                    new_var = NULL,
                    sep = NULL){

  grp_var_str <- rlang::as_name(rlang::ensym(grp_var))
  id_var_str  <- rlang::as_name(rlang::ensym(id_var))
  new_var_quo <- rlang::enquo(new_var)
  new_var_str <- if (rlang::quo_is_null(new_var_quo)) grp_var_str else rlang::as_name(new_var_quo)

  check_df(data, "data")
  check_varsindf(data, grp_var_str, "data", "grp_var")
  check_varsindf(data, id_var_str, "data", "id_var")

  data[[new_var_str]] <- var_addn(data, grp_var_str, id_var_str, sep)

  return(data)
}

# Internal helper: count distinct id_var per grp_var and return factor labels
var_addn <- function(data, grp_var_str, id_var_str, sep = NULL) {
  counts <- tapply(data[[id_var_str]], data[[grp_var_str]], dplyr::n_distinct)
  n <- unname(counts[as.character(data[[grp_var_str]])])
  parts <- if (is.null(sep)) paste(data[[grp_var_str]]) else paste(data[[grp_var_str]], sep)
  factor(paste(parts, paste0("(n=", n, ")")))
}
