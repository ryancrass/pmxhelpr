#' Internal helper: Resolve function input conflicts to standard internal string constants
#'
#' @param quo Function argument as a quosure (wrapped in rlang::ensym)
#' @param nullable Logical argument specifying if the argument may be `NULL`.
#'    Default is `FALSE`.
#'
#' @return A resolved string constant variable
#' @keywords internal
#' @examples
#' f <- function(x = DV) pmxhelpr:::resolve_var(rlang::enquo(x))
#' f()
#' f("ODV")
#' f(CONC)
#'
resolve_var <- function(quo, nullable = FALSE) {
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) return(expr)
  val <- tryCatch(
    list(value = rlang::eval_tidy(quo), ok = TRUE),
    error = function(e) list(value = NULL, ok = FALSE)
  )
  if (val$ok) {
    if (is.null(val$value) && nullable) return(NULL)
    if (is.character(val$value)) return(val$value)
  }
  if (is.symbol(expr)) return(as.character(expr))
  rlang::abort("Column specification must be a name or string")
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




#' Internal helper: Apply BLQ censoring rules to data
#'
#' @param data data.frame containing data to censor
#' @param pred_vars Character vector of prediction variable names (e.g., `"PRED"`, `"IPRED"`)
#'    to apply threshold-based BLQ imputation. Default is `NULL`.
#' @inheritParams plot_dvtime
#'
#' @return A data.frame with BLQ censoring applied
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' data_loq1 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 1)
#' data_loq2 <- pmxhelpr:::df_prep_blq(data, loq = 10, loq_method = 2)
#'
df_prep_blq <- function(data, loq, loq_method, pred_vars = NULL) {
  if(!loq_method %in% c(1, 2)) return(data)

  data <- data |>
    dplyr::mutate(LOQ = ifelse(is.null(loq), LLOQ, loq))

  if(loq_method == 1) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           MDV == 0 ~ DV,
                                           TIME <= 0 ~ 0,
                                           TIME > 0 ~ 0.5 * LOQ))
    for(v in pred_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          TIME <= 0 ~ 0,
          .data[[v]] >= LOQ ~ .data[[v]],
          .data[[v]] < LOQ ~ 0.5 * LOQ))
    }
  }

  if(loq_method == 2) {
    data <- data |>
      dplyr::mutate(DV = dplyr::case_when(EVID != 0 ~ NA_real_,
                                           MDV == 0 ~ DV,
                                           MDV == 1 ~ 0.5 * LOQ))
    for(v in pred_vars) {
      data <- data |>
        dplyr::mutate("{v}" := dplyr::case_when(
          EVID != 0 ~ NA_real_,
          .data[[v]] >= LOQ ~ .data[[v]],
          .data[[v]] < LOQ ~ 0.5 * LOQ))
    }
  }

  return(data)
}

#' Internal helper: Prepare data for plotting of a dependent variable versus time
#'
#' Validates inputs, renames time and output variables to internal standards,
#' applies BLQ imputation, and optionally applies dose normalization.
#'
#' @param data Input data.frame containing pharmacometric data.
#' @param time_var_str String name of the actual time column in `data`.
#' @param ntime_var_str String name of the nominal time column in `data`.
#' @param dv_var_str String name of the dependent variable column in `data`.
#' @param pred_var_str String name of the population prediction column in `data`, or `NULL`.
#' @param ipred_var_str String name of the individual prediction column in `data`, or `NULL`.
#' @param loq Numeric value of LLOQ, or `NULL`.
#' @param loq_method Integer (0, 1, or 2) specifying BLQ handling method.
#' @param dose_var_str String specifying the dose column name, or `NULL`.
#' @param col_var_str String specifying the color variable column name, or `NULL`.
#' @param id_var_str String specifying the group column name for spaghetti lines, or `NULL`.
#' @param dosenorm Logical indicating if dose normalization should be applied.
#' @param ref Numeric y-intercept for a horizontal reference line, or `NULL` for none.
#'
#' @return A named list with elements `data` (processed data.frame) and `lloq` (numeric LLOQ value).
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' prep <- pmxhelpr:::df_prep_dvtime(data, time_var_str = "TIME", ntime_var_str = "NTIME")
#' head(prep$data)
#'
df_prep_dvtime <- function(data,
                           time_var_str = "TIME",
                           ntime_var_str = "NTIME",
                           dv_var_str = "DV",
                           pred_var_str = NULL,
                           ipred_var_str = NULL,
                           dose_var_str = NULL,
                           col_var_str = NULL,
                           id_var_str = NULL,
                           loq = NULL,
                           loq_method = 0,
                           dosenorm = FALSE,
                           ref = NULL) {

  check_df(data, "data")
  check_varsindf(data, time_var_str, "data", "time_var")
  check_varsindf(data, ntime_var_str, "data", "ntime_var")
  check_varsindf(data, dv_var_str, "data", "dv_var")
  if (!is.null(pred_var_str)) check_varsindf(data, pred_var_str, "data", "pred_var")
  if (!is.null(ipred_var_str)) check_varsindf(data, ipred_var_str, "data", "ipred_var")
  check_varsindf(data, "MDV", "data", "MDV")
  if (!is.null(col_var_str)) {
    check_varsindf(data, col_var_str, "data", "col_var")
    check_factor(data, col_var_str, "col_var")
  }
  if (!is.null(id_var_str)) check_varsindf(data, id_var_str, "data", "id_var")
  if (isTRUE(dosenorm)) check_varsindf(data, dose_var_str, "data", "dose_var")
  check_loq_method(loq, loq_method, data)
  if (!is.null(ref)) check_numeric(ref, "ref")

  data <- df_prep_timevars(data, time_var_str, ntime_var_str)
  rename_vec <- c(DV = dv_var_str)
  if (!is.null(pred_var_str)) rename_vec <- c(rename_vec, PRED = pred_var_str)
  if (!is.null(ipred_var_str)) rename_vec <- c(rename_vec, IPRED = ipred_var_str)
  data <- dplyr::rename(data, dplyr::any_of(rename_vec))

  if (!is.null(dose_var_str) && dose_var_str != "DOSE") {
    data <- dplyr::rename(data, dplyr::any_of(c(DOSE = dose_var_str)))
  }

  if (!is.null(col_var_str)) {
    data[[col_var_str]] <- factor(data[[col_var_str]])
  }

  pred_vars <- c(if (!is.null(pred_var_str)) "PRED",
                  if (!is.null(ipred_var_str)) "IPRED")
  data <- df_prep_blq(data, loq, loq_method, pred_vars = pred_vars)

  lloq <- if ("LOQ" %in% colnames(data)) unique(data$LOQ[!is.na(data$LOQ)]) else NA_real_

  if (isTRUE(dosenorm)) {
    data$DV <- var_dosenorm(data$DV, data$DOSE)
    if (!is.null(pred_var_str)) data$PRED <- var_dosenorm(data$PRED, data$DOSE)
    if (!is.null(ipred_var_str)) data$IPRED <- var_dosenorm(data$IPRED, data$DOSE)
  }

  list(data = data, lloq = lloq)
}




#' Internal Helper: Apply dose-normalization to a variable
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param dose_var Vector containing dose
#'
#' @return A numeric vector of dose-normalized values of `dv_var`
#' @export var_dosenorm
#' @examples
#' data <- dplyr::mutate(data_sad, DNDV = var_dosenorm(ODV, DOSE))

var_dosenorm <- function(dv_var,
                         dose_var) {
  dv_var / dose_var
}


#' Internal Helper: Apply prediction correction
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param pred_var Vector containing population predictions (PRED)
#' @param lower_bound Lower bound for prediction correction formula.
#'
#' @return A numeric vector of prediction-corrected values of `dv_var`
#' @export var_pc
#' @examples
#' pkmodel <- model_mread_load(model = "pkmodel")
#' data <- df_mrgsim_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
#' data <- dplyr::mutate(data, PCDV = var_pc(ODV, PRED))
#'
var_pc <- function(dv_var,
                   pred_var,
                   lower_bound = 0) {
  predbin <- stats::median(pred_var)
  lower_bound + (dv_var - lower_bound) * ((predbin - lower_bound) / (pred_var - lower_bound))
}




#' Append counts of unique identifiers to group labels
#'
#' @description `var_addn()` counts distinct values of `id_var` within each
#'    level of `grp_var` and returns a factor with labels like `"100 mg (n=6)"`.
#'
#' @param grp_var Vector of grouping variable values.
#' @param id_var Vector of identifier values to count distinct entries of.
#' @param sep Optional separator to add between values of `grp_var` and appended counts.
#'    Default is NULL.
#'
#' @return A factor vector with group labels appended with subject counts (e.g., `"100 mg (n=6)"`)
#' @export var_addn
#' @examples
#' data <- dplyr::filter(data_sad, CMT != 3)
#' var_addn(data$DOSE, data$ID, sep = "mg")
#'
var_addn <- function(grp_var,
                     id_var,
                     sep = NULL) {
  counts <- tapply(id_var, grp_var, dplyr::n_distinct)
  n <- unname(counts[as.character(grp_var)])
  parts <- if (is.null(sep)) paste(grp_var) else paste(grp_var, sep)
  factor(paste(parts, paste0("(n=", n, ")")))
}





#' Internal Helper: Determine left-censoring for quantiles at the lower limit of quantification
#'
#' @param x Vector containing the variable to be censored
#' @param p Quantile for computation
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay
#'
#' @return Replaces values below loq (including NA) with -Inf, then computes
#   the quantile. Returns NA if the result is -Inf.
#' @keywords internal
#' @examples
#' data <- data_sad |>
#'   dplyr::group_by(CMT, NTIME, DOSE) |>
#'   dplyr::summarize(P05 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ),
#'                    P50 = pmxhelpr:::var_loqcens(ODV, 0.5, loq = LLOQ),
#'                    P95 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ), .groups = "drop")

var_loqcens <- function(x, p, loq) {
  x[is.na(x)] <- -Inf
  x[x < loq] <- -Inf
  q <- stats::quantile(x, probs = p, na.rm = TRUE)
  if (is.infinite(q) && q < 0) NA_real_ else as.numeric(q)
}
