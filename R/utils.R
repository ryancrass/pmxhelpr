#' Internal helper: Resolve function input conflicts to standard internal string constants
#'
#' Resolves a column argument to a string, in this order: a string literal is
#' used as is; a bare name found in `cols` is that column; otherwise the
#' expression is evaluated and a string result is used (so a variable holding
#' a column name works); failing that, a bare name is used as the column name.
#'
#' @param quo Function argument as a quosure (wrapped in rlang::ensym)
#' @param nullable Logical argument specifying if the argument may be `NULL`.
#'    Default is `FALSE`.
#' @param cols Names the argument is looked up in (e.g. `names(data)`, or a
#'    model's outputs), so a bare name matching one of them is never replaced
#'    by a same-named variable. `NULL` (default) skips that check.
#' @param call The call reported in the error.
#'
#' @return A resolved string constant variable
#' @keywords internal
#' @examples
#' f <- function(x = DV) pmxhelpr:::resolve_var(rlang::enquo(x))
#' f()
#' f("ODV")
#' f(CONC)
#'
resolve_var <- function(quo, nullable = FALSE, cols = NULL, call = rlang::caller_env()) {
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) return(expr)
  if (is.symbol(expr) && as.character(expr) %in% cols) return(as.character(expr))
  val <- tryCatch(
    list(value = rlang::eval_tidy(quo), ok = TRUE),
    error = function(e) list(value = NULL, ok = FALSE)
  )
  if (val$ok) {
    if (is.null(val$value) && nullable) return(NULL)
    if (is.character(val$value)) return(val$value)
  }
  if (is.symbol(expr)) return(as.character(expr))
  pmx_abort("Column specification must be a name or string.", "bad_arg", call = call)
}


#' Internal helper: Standardize time variable names in a data.frame
#'
#' Renames columns in `data` to the standardized `TIME` and `NTIME` names.
#' When both time variables map to the same source column, the column is
#' renamed to `NTIME` and `TIME` is created as a copy (or vice versa if the
#' source is already named `TIME`).
#'
#' @param data Input data.frame.
#' @param time_var_str String name of the actual time column in `data`.
#' @param ntime_var_str String name of the nominal time column in `data`.
#'
#' @return A data.frame with standardized `TIME` and `NTIME` columns
#' @keywords internal
#' @examples
#' data <- dplyr::rename(data_sad, NTFD = NTIME)
#' c("TIME", "NTIME") %in% colnames(data)
#' data_std <- pmxhelpr:::df_prep_timevars(data, "TIME", "NTFD")
#' c("TIME", "NTIME") %in% colnames(data_std)
#'
df_prep_timevars <- function(data, time_var_str, ntime_var_str) {
  if (time_var_str == ntime_var_str) {
    data <- dplyr::rename(data, dplyr::any_of(c(NTIME = ntime_var_str)))
    if (!"TIME" %in% colnames(data)) data$TIME <- data$NTIME
  } else {
    data <- dplyr::rename(data, dplyr::any_of(c(TIME = time_var_str,
                                                 NTIME = ntime_var_str)))
  }
  data
}


#' Internal helper: Remove NULL entries from a list
#'
#' @param x A named list potentially containing NULL values.
#'
#' @return A list with all NULL entries removed
#' @keywords internal
#' @examples
#' pmxhelpr:::compact(list(a = 1, b = NULL, c = 3))
#'
compact <- function(x) x[!vapply(x, is.null, logical(1))]


